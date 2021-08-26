# How to use ei to marshal binary terms in port programs

**Author**: Serge Aleynikov <saleyn at gmail.com>

## Overview
  
The purpose of this tutorial is to illustrate how to use ei interface library (C API) for dealing with the Erlang binary term format used in marshaling data between an Erlang node and external port programs.

The reader is also encouraged to read another how to guide by Pete Kazmer that focuses on using OTP framework for building Erlang systems that interface with external programs.

ei offers a simple and convenient interface that makes it possible to communicate with Erlang from virtually any language that can call native C functions.

At the time of this writing there are two foreign function interface (FFI) models available in Erlang:

* In-process port driver.
* BIF library interface
* Out-of-process port program.
  
The first two are obviously much faster, but are more complicated and have more dangerous impact on stability of the virtual machine - a sloppy implementation can easily crash the emulator. The second approach is slower but offers a guaranteed isolation of port program's crashes from bringing down the emulator. The decision of which interface to use is case-by-case specific, however, unless you are implementing high-performance low-latency components, you will find second approach being adequate in many cases.

In this tutorial we'll illustrate the simplicity of the third approach to build a port program in C.

## Introduction
  
The earlier releases of Erlang/OTP came with erl_interface library that later evolved into ei (Erlang Interface). The difference between ei and erl_interface is that, in contrast to erl_interface, ei uses external binary term format directly when sending and receiving terms in communications with an Erlang node, and it can use a given memory buffer without involving dynamic memory. While ei seems to be the preferred method of encoding/decoding terms, most of the help documents provided with Erlang describing how to build port programs still use erl_interface function calls or raw byte-coding methods as examples.

This tutorial demonstrates the use of ei's encoding and decoding functions for marshaling data between an Erlang process and a C program.

In order to focus this tutorial on ei rather than on details of OTP, we'll use only basic Erlang functionality in building the interface process communicating with the C program. The example described in this tutorial will show how to implement functions add(X, Y), multiply(X, Y) and divide(X, Y) that can be called from Erlang and executed in C.

## Marshaling Protocol
First, we need to define the data marshaling contract between Erlang and C that will request from C to execute functions and receive the result back.

The objective of the port program is to be able to implement the following API:
```
Erlang -> C            C -> Erlang
===========            ===========

{add,      X, Y}  ->   {ok, Result::integer()} | {error, Reason::atom()}
{multiply, X, Y}  ->   {ok, Result::integer()} | {error, Reason::atom()}
{divide,   A, B}  ->   {ok, Result::float()}   | {error, Reason::atom()}
  X = integer()
  Y = integer()
  A = integer() | float()
  B = integer() | float()
```
Where the add, multiply, and divide instructions perform the corresponding action on the C-side given two arguments.

## Erlang side
Let's start with writing an Erlang module responsible for marshaling data to and from the C port program. The module will use erlang:port_command/2 function for asynchronously sending data to the port. Note that the data is encoded to the external binary term format using erlang:term_to_binary/1.

Erlang module listing
```erlang
-module(port).

% API
-export([start/0, start/1, stop/0, add/2, multiply/2, divide/2]).
% Internal exports
-export([init/1]).

start() ->
    start("./cport").
start(ExtPrg) ->
    spawn_link(?MODULE, init, [ExtPrg]).
stop() ->
    ?MODULE ! stop.

add(X, Y) ->
    call_port({add, X, Y}).
multiply(X, Y) ->
    call_port({multiply, X, Y}).
divide(X, Y) ->
    call_port({divide, X, Y}).

call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
    Result ->
        Result
    end.

init(ExtPrg) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary, exit_status]),
    loop(Port).

loop(Port) ->
    receive
    {call, Caller, Msg} ->
        io:format("Calling port with ~p~n", [Msg]),
        erlang:port_command(Port, term_to_binary(Msg)),
        receive
        {Port, {data, Data}} ->
            Caller ! binary_to_term(Data);
        {Port, {exit_status, Status}} when Status > 128 ->
            io:format("Port terminated with signal: ~p~n", [Status-128]),
            exit({port_terminated, Status});
        {Port, {exit_status, Status}} ->
            io:format("Port terminated with status: ~p~n", [Status]),
            exit({port_terminated, Status});
        {'EXIT', Port, Reason} ->
            exit(Reason)
        end,
        loop(Port);
    stop ->
        erlang:port_close(Port),
        exit(normal)
    end.
```

## C side
  
The port program that implements the API functions consists of two parts. Part I is the actual loop that decodes data from Erlang, calls the API functions, and encodes the result back to be sent to Erlang. This part uses ei interface library, and is the main focus of this turorial. Part II implements the data marshaling procedures to read and write data from standard input and to standard output.

ei library implements a number of "ei_decode*()" functions. They can be used on the data received from Erlang that is encoded using erlang:term_to_binary/1 function call. The ei_decode_version() function is used to make sure that the incoming term is an external binary representation, and that the version magic number for the erlang binary term format is decoded. The rest of the ei_decode_* functions are used to decode tuple, integer, and double representation of data passed to the C port program.

The result to be sent back to Erlang is encoded using a dynamic buffer of ei_x_buff type. We could've used a static buffer and handled memory management ourselves, but for the sake of this tutorial it's worth showing how the buffer can be managed automatically by using ei_x_* function calls. The output stored in external binary term representation is being initialized using ei_x_new_with_version() call, and ei_x_encode_tuple_header() is used to create a placeholder for the {ok, Result} or {error, Reason} tuple.

### C Program listing
```c
#include <ei.h>

#include <unistd.h>
#include <sys/io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 128 

typedef unsigned char byte;

int read_cmd(byte **buf, int *size);
int write_cmd(ei_x_buff* x);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

/*-----------------------------------------------------------------
 * API functions
 *----------------------------------------------------------------*/
long add(long a, long b) {
  return a + b;
}

long multiply(long a, long b) {
  return a * b;
}

double divide(double a, double b) {
  return a / b;
}

/*-----------------------------------------------------------------
 * MAIN
 *----------------------------------------------------------------*/
int main() {
  byte*     buf;
  int       size = BUF_SIZE;
  char      command[MAXATOMLEN];
  int       index, version, arity;
  long      a, b, c;
  double    x, y, z;
  ei_x_buff result;

  #ifdef _WIN32
  /* Attention Windows programmers: you need to explicitly set
   * mode of stdin/stdout to binary or else the port program won't work
   */
  setmode(fileno(stdout), O_BINARY);
  setmode(fileno(stdin), O_BINARY);
  #endif

  if ((buf = (byte *) malloc(size)) == NULL) 
    return -1;
    
  while (read_cmd(&buf, &size) > 0) {
    /* Reset the index, so that ei functions can decode terms from the 
     * beginning of the buffer */
    index = 0;

    /* Ensure that we are receiving the binary term by reading and 
     * stripping the version byte */
    if (ei_decode_version(buf, &index, &version)) return 1;
    
    /* Our marshalling spec is that we are expecting a tuple {Command, Arg1, Arg2} */
    if (ei_decode_tuple_header(buf, &index, &arity)) return 2;
    
    if (arity != 3) return 3;
    
    if (ei_decode_atom(buf, &index, command)) return 4;
    
    /* Prepare the output buffer that will hold {ok, Result} or {error, Reason} */
    if (ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return 5;
    
    if (!strcmp("add", command) || !strcmp("multiply", command)) {
      if (ei_decode_long(buf, &index, &a)) return 6;
      if (ei_decode_long(buf, &index, &b)) return 7;

      if (!strcmp("add", command))
        c = add(a, b);
      else
        c = multiply(a, b);

      if (ei_x_encode_atom(&result, "ok") || ei_x_encode_long(&result, c)) return 8;
      
    } else if (!strcmp("divide", command)) {
      /* Allow incoming parameters to be integers or floats */
      if (!ei_decode_long(buf, &index, &a))
        x = a;
      else if (ei_decode_double(buf, &index, &x)) 
        return 6;
        
      if (!ei_decode_long(buf, &index, &b))
        y = b;
      else if (ei_decode_double(buf, &index, &y)) 
        return 7;
      
      if (y == 0.0) {
        if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "division_by_zero")) 
          return 8;
      } else {
        z = divide(x, y);
        if (ei_x_encode_atom(&result, "ok") || ei_x_encode_double(&result, z)) 
          return 8;
      }
      
    } else {
      if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "unsupported_command")) 
        return 99;
    }

    write_cmd(&result);

    ei_x_free(&result);
  }
  return 0;
}

/*-----------------------------------------------------------------
 * Data marshalling functions
 *----------------------------------------------------------------*/
int read_cmd(byte **buf, int *size)
{
  int len;

  if (read_exact(*buf, 2) != 2)
    return(-1);
  len = (*buf[0] << 8) | *buf[1];

  if (len > *size) {
    byte* tmp = (byte *) realloc(*buf, len);
    if (tmp == NULL)
      return -1;
    else
      *buf = tmp;
    *size = len;
  }
  return read_exact(*buf, len);
}

int write_cmd(ei_x_buff *buff)
{
  byte li;

  li = (buff->index >> 8) & 0xff; 
  write_exact(&li, 1);
  li = buff->index & 0xff;
  write_exact(&li, 1);

  return write_exact(buff->buff, buff->index);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return i;
    got += i;
  } while (got<len);

  return len;
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote<len);

  return len;
}
```

### Data Types
  
Though in this example we didn't use ei_get_type() function, this function is quite useful when there's a need to determine the type of the next encoded term in the buffer. A question that gets frequently asked is which include file erl_interface.h or ei.h lists the macros that can be used to represent types returned by the ei_get_type() call? These macros are defined in the ei.h header:
```c
#define ERL_SMALL_INTEGER_EXT 'a'
#define ERL_INTEGER_EXT       'b'
#define ERL_FLOAT_EXT         'c'
#define ERL_ATOM_EXT          'd'
#define ERL_REFERENCE_EXT     'e'
#define ERL_NEW_REFERENCE_EXT 'r'
#define ERL_PORT_EXT          'f'
#define ERL_PID_EXT           'g'
#define ERL_SMALL_TUPLE_EXT   'h'
#define ERL_LARGE_TUPLE_EXT   'i'
#define ERL_NIL_EXT           'j'
#define ERL_STRING_EXT        'k'
#define ERL_LIST_EXT          'l'
#define ERL_BINARY_EXT        'm'
#define ERL_SMALL_BIG_EXT     'n'
#define ERL_LARGE_BIG_EXT     'o'
#define ERL_NEW_FUN_EXT       'p'
#define ERL_FUN_EXT       'u'
```

### Makefile
The following makefile can be used to build the sample port program and the corresponding Erlang module:

```make
$ cat Makefile
ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.5.5
CFLAGS=-Wall -I/usr/local/include -I$(ERL_LIB)/include
LDFLAGS=-L. -L$(ERL_LIB)/lib 

all: cport port.beam

cport: cport.o
        gcc $(LDFLAGS) $< -lerl_interface -lei -lpthread -o $@

%.o: %.c
        gcc -g $(CFLAGS) -o $@ -c $<

%.beam: %.erl
        erlc $<

clean:
        rm port.beam cport cport.o
```

### Running example

Running make with the Makefile above will create two files:
```
cport
port.beam
```
Now, let's start the emulator, and verify that everything works.

### Running the port program
```erlang
$ erl        
Erlang (BEAM) emulator version 5.5 [source] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.5  (abort with ^G)
1> port:start().
<0.35.0>
2> port:add(10,5).
Calling port with {add,10,5}
{ok,15}
3> port:multiply(3,6).
Calling port with {multiply,3,6}
{ok,18}
4> port:divide(10,5).
Calling port with {divide,10,5}
{ok,2.00000}
5> port:divide(10,0).
Calling port with {divide,10,0}
{error,division_by_zero}
6> port:stop().
stop
7> 
```
  
## Conclusion
As was illustrated in this tutorial ei provides a set of functions for convenient encoding and decoding of the erlang binary term format that can be used in other languages for interfacing with Erlang.
