# Building a Non-blocking TCP server using OTP principles

**Author**: Serge Aleynikov <saleyn at gmail.com>

## Overview
  
A reader of this tutorial is assumed to be familiar with gen_server and gen_fsm behaviours, TCP socket communications using gen_tcp module, active and passive socket modes, and OTP supervision principles.

OTP provides a convenient framework for building reliable applications. This is in part accomplished by abstracting common functionality into a set of reusable behaviours such as gen_server and gen_fsm that are linked to OTP's supervision hierarchy.

There are several known TCP server designs. The one we are going to cover involves one process listening for client connections and spawning an FSM process per connecting client. While there is support for TCP communications available in OTP through the gen_tcp module, there is no standard behavior for building non-blocking TCP servers using OTP standard guidelines. By non-blocking we imply that the listening process and the client-handling FSMs should not make any blocking calls and be readily responsive to incoming control messages (such as changes in system configuration, restart requests, etc.) without causing timeouts. Note that blocking in the context of Erlang means blocking an Erlang process rather than the emulator's OS process(es).

In this tutorial we will show how to build a non-blocking TCP server using gen_server and gen_fsm behaviours that offers flow control and is fully compliant with OTP application design principles.

A reader who is new to the OTP framework is encouraged to read Joe Armstrong's tutorial on how to build A Fault-tolerant Server using blocking gen_tcp:connect/3 and gen_tcp:accept/1 calls without involving OTP.

This tutorial was inspired by several threads (e.g. one, two) on the Erlang Questions mailing list mentioning an approach to building non-blocking asynchronous TCP servers.

## Server Design

The design of our server will include the main application's supervisor tcp_server_app process with one_for_one restart strategy and two child specifications. The first one being a listening process implemented as a gen_server behaviour that will wait for asynchronous notifications of client socket connections. The second one is another supervisor tcp_client_sup responsible for starting client handling FSMs and logging abnormal disconnects via standard SASL error reports.

For the sake of simplicity of this tutorial, the client handling FSM (tcp_echo_fsm) will implement an echo server that will echo client's requests back to the client.
```
                 +----------------+
                 | tcp_server_app |
                 +--------+-------+
                          | (one_for_one)
         +----------------+---------+
         |                          |
 +-------+------+           +-------+--------+
 | tcp_listener |           + tcp_client_sup |
 +--------------+           +-------+--------+
                                    | (simple_one_for_one)
                              +-----|---------+
                            +-------|--------+|
                           +--------+-------+|+
                           |  tcp_echo_fsm  |+
                           +----------------+
```
## Application and Supervisor behaviours
  
In order to build an OTP application we need to construct modules implementing an application and supervisor behaviour callback functions. While traditionally these functionalities are implemented in separate modules, given their succinctness we'll combine them in one module.

As an added bonus we implement a get_app_env function that illustrates how to process configuration options as well as command-line options given to the emulator at start-up.

The two instances of init/1 function are for two tiers of supervision hierarchy. Since two different restart strategies for each supervisor are needed, we implement them at different tiers.

Upon application's startup the tcp_server_app:start/2 callback function calls supervisor:start_link/2 that creates main application's supervisor calling tcp_server_app:init([Port, Module]) callback. This supervisor creates a tcp_listener process and a child supervisor tcp_client_sup responsible for spawning client connections. The Module argument in the init function is the name of client-connection handling FSM (in this case tcp_echo_fsm).

### TCP Server Application (tcp_server_app.erl)

```erlang
-module(tcp_server_app).
-author('saleyn@gmail.com').

-behaviour(application).

%% Internal API
-export([start_client/0]).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
    supervisor:start_child(tcp_client_sup, []).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, tcp_echo_fsm]).

stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Listener
              {   tcp_server_sup,                          % Id       = internal id
                  {tcp_listener,start_link,[Port,Module]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [tcp_listener]                           % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   tcp_client_sup,
                  {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [Module]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.

### Listener Process
  
One of the shortcomings of the gen_tcp module is that it only exports interface to a blocking accept call. This leads most of developers working on an implementation of a TCP server build a custom process linked to a supervisor using proc_lib or come up with some other proprietary design.

Examining prim_inet module reveals an interesting fact that the actual call to inet driver to accept a client socket is asynchronous. While this is a non-documented property, which means that the OTP team is free to change this implementation, we will exploit this functionality in the construction of our server.

The listener process is implemented as a gen_server behaviour:

### TCP Listener Process (tcp_listener.erl)

```erlang
-module(tcp_listener).
-author('saleyn@gmail.com').

-behaviour(gen_server).

%% External API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
                listener,       % Listening socket
                acceptor,       % Asynchronous acceptor's internal reference
                module          % FSM handling module
               }).

%%--------------------------------------------------------------------
%% @spec (Port::integer(), Module) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------
init([Port, Module]) ->
    process_flag(trap_exit, true),
    Opts = [binary, {packet, 2}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
    {ok, Listen_socket} ->
        %%Create first accepting process
        {ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
        {ok, #state{listener = Listen_socket,
                    acceptor = Ref,
                    module   = Module}};
    {error, Reason} ->
        {stop, Reason}
    end.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state{listener=ListSock, acceptor=Ref, module=Module} = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
        ok              -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
        end,

        %% New client connected - spawn a new process using the simple_one_for_one
        %% supervisor.
        {ok, Pid} = tcp_server_app:start_client(),
        gen_tcp:controlling_process(CliSocket, Pid),
        %% Instruct the new FSM that it owns the socket.
        Module:set_socket(Pid, CliSocket),

        %% Signal the network driver that we are ready to accept another connection
        case prim_inet:async_accept(ListSock, -1) of
        {ok,    NewRef} -> ok;
        {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

        {noreply, State#state{acceptor=NewRef}}
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
        {stop, Why, State}
    end;

handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.
```
  
In this module init/1 call takes two parameters - the port number that the TCP listener should be started on and the name of a protocol handling module for client connections. The initialization function opens a listening socket in passive {active, false} mode. This is done so that we have flow control of the data received on the connected client sockets that will inherit this option from the listening socket.

The most interesting part of this code is the prim_inet:async_accept/2 call as well as the handling of asynchronous inet_async messages. In order to get this working we also needed to copy some of the internal OTP code encapsulated in the set_sockopt/2 function that handles socket registration with inet database and copying some options to the client socket.

As soon as a client socket is connected inet driver will notify the listening process using {inet_async, ListSock, Ref, {ok, CliSocket}} message. At this point we'll instantiate a new client socket handling process and set its ownership of the CliSocket.

### Client Socket Handling Process
  
While tcp_listener is a generic implementation, tcp_echo_fsm is a mere stub FSM for illustrating how to write TCP servers. This modules needs to export two functions - one start_link/0 for a tcp_client_sup supervisor and another set_socket/2 for the listener process to notify the client connection handling FSM process that it is now the owner of the socket, and can begin receiving messages by setting the {active, once} or {active, true} option.

We would like to highlight the synchronization pattern used between the listening process and client connection-handling FSM to avoid possible message loss due to dispatching some messages from the socket to the wrong (listening) process. The process owning the listening socket has it open with {active, false}. After accepting the client's socket that socket inherits its socket options (including {active, false}) from the listener, transfers ownership of the socket to the newly spawned client connection-handling FSM by calling gen_tcp:controlling_process/2 and calls Module:set_socket/2 to notify the FSM that it can start receiving messages from the socket. Until the FSM process enables message delivery by setting the active mode on the socket by calling inet:setopts(Socket, [{active, once}]), the data sent by the TCP sender stays in the socket buffer.

When socket ownership is transfered to FSM in the 'WAIT_FOR_SOCKET' state the FSM sets {active, once} option to let inet driver send it one TCP message at a time. This is the OTP way of preserving flow control and avoiding process message queue flooding with TCP data and crashing the system in case of a fast-producer-slow-consumer case.

The FSM states are implemented by special functions in the tcp_echo_fsm module that use a naming convention with capital case state names enclosed in single quotes. The FSM consists of two states. 'WAIT_FOR_SOCKET' is the initial state in which the FSM is waiting for assignment of socket ownership, and 'WAIT_FOR_DATA' is the state that represents awaiting for TCP message from a client. In this state FSM also handles a special 'timeout' message that signifies no activity from a client and causes the process to stop and close client connection.

### TCP Client Socket Handling FSM (tcp_echo_fsm.erl)

```erlang
-module(tcp_echo_fsm).
-author('saleyn@gmail.com').

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_DATA'/2
]).

-record(state, {
                socket,    % client socket
                addr       % client address
               }).

-define(TIMEOUT, 120000).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'WAIT_FOR_DATA', State#state{socket=Socket, addr=IP}, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #state{socket=S} = State) ->
    ok = gen_tcp:send(S, Data),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_DATA'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
Application File
Another required part of building an OTP application is creation of an application file that includes application name, version, startup module and environment.

Application File (tcp_server.app)

{application, tcp_server,
 [
  {description, "Demo TCP server"},
  {vsn, "1.0"},
  {id, "tcp_server"},
  {modules,      [tcp_listener, tcp_echo_fsm]},
  {registered,   [tcp_server_sup, tcp_listener]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {tcp_server_app, []}},
  {env, []}
 ]
}.
```
  
### Compiling

Create the following directory structure for this application:
```bash
 ./tcp_server
 ./tcp_server/ebin/
 ./tcp_server/ebin/tcp_server.app
 ./tcp_server/src/tcp_server_app.erl
 ./tcp_server/src/tcp_listener.erl
 ./tcp_server/src/tcp_echo_fsm.erl
 $ cd tcp_server/src
 $ for f in tcp*.erl ; do erlc +debug_info -o ../ebin $f
```
  
### Running

We are going to start an Erlang shell with SASL support so that we can view all progress and error reports for our TCP application. Also we are going to start appmon application in order to examine visually the supervision hierarchy.

```erlang
 $ cd ../ebin
 $ erl -boot start_sasl
 ...
 1> appmon:start().
 {ok,<0.44.0>}
 2> application:start(tcp_server).
 ok
```
Now click on the tcp_server button in the appmon's window in order to display supervision hierarchy of the tcp_server application.
```erlang
 3> {ok,S} = gen_tcp:connect({127,0,0,1},2222,[{packet,2}]).
 {ok,#Port<0.150>}
```
The step above initiated a new client connection to the echo server.
```erlang
 4> gen_tcp:send(S,<<"hello">>).
 ok
 5> f(M), receive M -> M end.
 {tcp,#Port<0.150>,"hello"}
```
We verified that the echo server works as expected. Now let's try to crash the client connection on the server and watch for the supervisor generating an error report entry on screen.
```erlang
 6> [{_,Pid,_,_}] = supervisor:which_children(tcp_client_sup).
 [{undefined,<0.64.0>,worker,[]}]
 7> exit(Pid,kill).
 true
 =SUPERVISOR REPORT==== 31-Jul-2007::14:33:49 ===
      Supervisor: {local,tcp_client_sup}
      Context:    child_terminated
      Reason:     killed
      Offender:   [{pid,<0.77.0>},
                   {name,undefined},
                   {mfa,{tcp_echo_fsm,start_link,[]}},
                   {restart_type,temporary},
                   {shutdown,2000},
                   {child_type,worker}]
```
Note that if you are putting this server under a stress test with many incoming connections, the listener process may fail to accept new connections after the number of open file descriptors reaches the limit set by the operating system. In that case you will see the error:
"too many open files"
If you are running Linux/UNIX, google for a solution (which ultimately boils down to increasing the per-process limit by setting "ulimit -n ..." option).

## Conclusion
OTP provides building blocks for constructing non-blocking TCP servers. This tutorial showed how to create a simple TCP server with flow control using standard OTP behaviours. As an exercise the reader is encouraged to try abstracting generic non-blocking TCP server functionality into a stand-along behaviour.

## Sample Implementations

- [Proto lib on GitHub](https://github.com/saleyn/proto)
- [gen_socket on GitHub](http://github.com/erlware/gen_socket)
- [gen_listener_tcp on GitHub](http://github.com/kaos/gen_listener_tcp/tree)
- [A protocol agnostic tcp/ip gateway that runs in the upper layer of the OSI stack](https://github.com/lafka/linedancer)
