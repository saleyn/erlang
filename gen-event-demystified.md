# Gen event behavior demystified

**Author**: Serge Aleynikov <saleyn at gmail.com>

##Overview
  
Gen_event is one of the standard OTP behaviours included in the Erlang/OTP distribution. It is not as frequently used as gen_server and hence often is not well understood by developers who begin learning the OTP.

The purpose of this tutorial is to reveal the mistery of gen_event, and to demonstrate how to build fault tolerate event handlers.

This tutorial assumes that the reader is familiar with the OTP and other standard behaviors, such as gen_server.

## Gen_event Behavior
  
As with the other OTP behaviors, gen_event abstracts generic properties from specific properties of an implementation and allows a developer to extend a system with custom event-handling hooks.

Those with Object-Oriented background in languages such as C#, might be familier with a concept of a delegate. Delegates allow adding one or more callback functions that will be called in respective order upon processing a certain event. They can be added and removed dynamically at run time. A typical example of a delegate is that upon clicking an OK button on a GUI form, one may want to execute some validation of the form fields' content. We can accomplish that by adding one or more delegates to the onClick method of the OK button.

Gen_event behavior allows to implement a similar approach - to have a process responsible for interacting with an OK button on some GUI form, which may allow to dynamically add callback modules that will receive notifications such as {button, pressed}.

The reader at this point may ask a reasonable question: "This looks very similar to the gen_server behavior! How are they different and why do we need a separate behavior that can be emulated with gen_server?"

While gen_server and gen_event seem to be similar, they are fundamentally different. In order to understand this difference let's examine the context in which both behaviors execute the callback code.

Gen_server is usually spawned as a separate process, and all user callbacks (such as Mod:handle_call/3, Mod:handle_cast/2, etc) are executed in the context of that process.

In contrast, gen_event can be percieved as two sets of components:

- Gen_event manager process.
- Event handler modules.
  
A gen_event manager is the process that registers callbacks. It is very important to understand that all events dispatched by the event manager process to event handlers execute in the context of the event manager process. Event handler modules allow to add custom handlers to the list of event handlers maintained by the event manager.

Since an event manager executes event handlers in its own context, it itself guards failures in the custom callback code, and therefore is more fault tolerant than gen_server. What happens if an added custom event_handler doesn't process callback event correctly and results in an exception being raised? Then the event manager removes that event handler from the internal list of registered callbacks.

Three typical examples of this principle in OTP are the error_logger, alarm_handler, and error_handler event managers. All error events reported by the OTP framework are dispatched through the error_logger. By default at node startup a basic event hander module is added to the error_logger event manager that merely prints an error report to the shell. More sophisticated event loggers (such as the one installed by SASL application) replace the basic event handler, and allow to print more compehensive error reports to display or to file. The reader is encouraged to examine the LAMA application in jungerl that utilizes this principle by replacing SASL's error_logger handler with its own and sending error reports to syslog or other destinations.

## Fault Tolerance
After reading the paragraphs above the reader may ask a question: "Hmm, it all sounds great, but what if there is a fault in the custom callback module and the event manager removes the callback from its list of event_handler modules? Will this happen silently, and if so, how can we reinstall a failed handler to ensure proper operation of the system?"

The removal of a faulty event handler is a silent operation. It does produce an error report printed on display, however, common monitoring techniques (such as link or monitor primitives) cannot be used on an event handler. Why not? Let's emphasize this again - because the handler code is executed in the context of the event manager, and links or monitors would only be able to help if the event manager crashed, but a faulty event_handler's code doesn't crash the event manager process.

If that's being the case how do we build fault-tolerant systems containing event handlers in presense of software errors in the event handler code? The answer can be found by examining the gen_event's list of exported functions. Gen_event exports the following function:
```erlang
add_sup_handler(EventMgrRef, Handler, Args) -> Result
```
If the event handler is deleted due to a fault, the event manager sends a message {gen_event_EXIT,Handler,Reason} to the calling process. So, how does it help us build fault tolerant code? We need to spawn a separate process that would be responsible for adding our custom event_handler callback module using gen_event:add_sup_handler/3 to an event manager (that process can itself be a gen_server), and upon receiving a {gen_event_EXIT,Handler,Reason} message from the event manager reinstalling the event handler.

Let's illustrate this by an example:

## Event handler guard

```erlang
$ cat handler_guard.erl
-module(handler_guard).

-export([start_link/1]).

% gen_server's callbacks
-export([init/1, handle_info/2]).

-behavior(gen_server).

start_link(HandlerModule, Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [HandlerModule, Options], []).

init([HandlerModule, Options]) ->
    % Call the new event handler's startup procedure
    case catch HandlerModule:start(Options) of
    ok ->
        {ok, HandlerModule};
    already_started ->
        {stop, {already_started, HandlerModule}};
    Error ->
        {stop, Error}
    end.

handle_info({gen_event_EXIT, HandlerModule, Reason}, HandlerModule) ->
    %% gen_event manager sends this message if a handler was added using
    %% gen_event:add_sup_handler/3 or gen_event:swap_sup_handler/3 functions
    io:format("~w: detected handler ~p shutdown:~n~p~n",
              [?MODULE, HandlerModule, Reason]),
    {stop, {handler_died, HandlerModule, Reason},HandlerModule};

handle_info(Other, HandlerModule) ->
    %% This process should not receive other messages
    io:format("~w: received unknown message:~n~p~n", [?MODULE, Other]),
    {noreply, HandlerModule}.
Custom event handler module


$ cat my_alarm_handler.erl
-module(my_alarm_handler).

%% API 
-export([start/1, stop/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

start(Options) ->
    % gen_event:add_handler/2 doesn't check for duplicates
    case lists:member(?MODULE, gen_event:which_handlers(alarm_handler)) of
    true  ->
        already_started;
    false ->
        case gen_event:swap_sup_handler(alarm_handler,
               {alarm_handler, swap}, {?MODULE, Options}) of
        ok -> 
            ok;
        {error, Reason} ->
            throw({error, {?MODULE, start_link, Reason}})
        end
    end.

stop() ->
    gen_event:swap_handler(alarm_handler, {?MODULE, swap}, {alarm_handler, []}).

%% init/1 is called when a event is being installed to an event manager
%% using gen_event:add_[sup_]handler/3 function
init({_Options, OldAlarms}) ->
    {ok, OldAlarms}.

handle_event({Type, Alarm}, Alarms) when Type=:=set_alarm; Type=:= clear_alarm ->
    log_alarm(Type, Alarm),
    {ok, NewState};

handle_event(_, Alarms)->
    {ok, Alarms}.

handle_call(_Query, Alarms) -> {ok, {error, bad_query}, Alarms}.
handle_info(_, Alarms)      -> {ok, Alarms}.

%% terminate/2 is called when
%% gen_event:swap_handler(EventMgr, {?MODULE, swap}, {NewModule, Args}) is invoked
terminate(swap,    Alarms)  -> {?MODULE, Alarms};
terminate(_Reason,_Alarms)  -> ok.

log_alarm(Type, Alarm) when Type==set_alarm; Type==clear_alarm ->
    % do custom logging here
    io:format("Custom alarm log function invoked for %w: %p~n", [Type, Alarm]),
    ok.
```
In this example the my_alarm_handler module is a new event handler that replaces default alarm_handler. This module merely displays the alarm information on the screen, but could be easily expanded to log alarms to a database, syslog or email by changing implementation of the log_alarm/2 function.

The handler_guard module can be linked to an application's supervisor, and is responsible for restarting the custom event handler at startup or when a fault causes removal of the event handler by the event manager process. The reinstallation of the handler is accomplished by the fact that when handler_guard is terminated with {stop, Reason}, its supervisor will restart the process that will in turn re-register alarm_handler callback.

## Conclusion
This tutorial explained the details of gen_event behavior available in OTP. It also showed how to implement fault-tolerant event handlers.
