-module(igor_types).

%% Include files

%% Exported functions

-export([]).

-export_type([
    dict/2,
    context/0,
    context_id/0,
    rpc_id/0,
    integer_type/0,
    float_type/0,
    primitive_type/0,

    sbyte/0,
    short/0,
    ushort/0,
    int/0,
    uint/0,
    long/0,
    ulong/0
]).

-type dict(Key, Value) :: [{Key, Value}].
-type context() :: term().
-type context_id() :: integer().
-type rpc_id() :: integer().

-type integer_type() :: 'sbyte' | 'byte' | 'short' | 'ushort' | 'int' | 'uint' | 'long' | 'ulong'.
-type float_type() :: 'float' | 'double'.
-type primitive_type() :: 'boolean' | integer_type() | float_type() | 'binary' | 'string' | 'atom'.

-type sbyte() :: -16#80 .. 16#7f.
-type short() :: -16#8000 .. 16#7fff.
-type ushort() :: 0 .. 16#ffff.
-type int() :: -16#80000000 .. 16#7fffffff.
-type uint() :: 0..16#ffffffff.
-type long() :: -16#8000000000000000 .. 16#7fffffffffffffff.
-type ulong() :: 0..16#ffffffffffffffff.

%% API

%% Local functions
