-module(igor_string).

%% Include files

-include("igor_http.hrl").
-include("igor.hrl").

%% Exported functions

-export([
    parse/2,
    parse/3,
    format/2,
    parse_value/2,
    format_value/2
]).

%% API

parse(Value, {option, Type}) ->
    parse(Value, Type, undefined);
parse(undefined, _Type) ->
    error(badarg);
parse(Value, Type) when is_binary(Value) ->
    parse_value(Value, Type).

parse(undefined, _Type, Default) ->
    Default;
parse(Value, Type, _Default) ->
    parse_value(Value, Type).

format(Value, {option, Type}) ->
    format_value(Value, Type);
format(Value, Type) ->
    format_value(Value, Type).

parse_value(<<"true">>, boolean) -> true;
parse_value(<<"false">>, boolean) -> false;
parse_value(Bin, sbyte) -> binary_to_integer(Bin);
parse_value(Bin, byte) -> binary_to_integer(Bin);
parse_value(Bin, short) -> binary_to_integer(Bin);
parse_value(Bin, ushort) -> binary_to_integer(Bin);
parse_value(Bin, int) -> binary_to_integer(Bin);
parse_value(Bin, uint) -> binary_to_integer(Bin);
parse_value(Bin, long) -> binary_to_integer(Bin);
parse_value(Bin, ulong) -> binary_to_integer(Bin);
parse_value(Bin, float) -> binary_to_float_or_int(Bin);
parse_value(Bin, double) -> binary_to_float_or_int(Bin);
parse_value(Bin, string) -> Bin;
parse_value(Bin, binary) -> Bin;
parse_value(Bin, atom) -> binary_to_atom(Bin, latin1);
parse_value(Bin, {list, Separator, ItemType}) -> [ parse_value(Item, ItemType) || Item <- binary:split(Bin, Separator, [global]) ];
parse_value(Bin, {custom, Fun}) -> Fun(Bin).

format_value(true, boolean) -> <<"true">>;
format_value(false, boolean) -> <<"false">>;
format_value(Value, sbyte) when is_integer(Value) -> integer_to_list(Value);
format_value(Value, byte) when is_integer(Value) -> integer_to_list(Value);
format_value(Value, short) when is_integer(Value) -> integer_to_list(Value);
format_value(Value, ushort) when is_integer(Value) -> integer_to_list(Value);
format_value(Value, int) when is_integer(Value) -> integer_to_list(Value);
format_value(Value, uint) when is_integer(Value) -> integer_to_list(Value);
format_value(Value, long) when is_integer(Value) -> integer_to_list(Value);
format_value(Value, ulong) when is_integer(Value) -> integer_to_list(Value);
format_value(Value, float) when is_number(Value) -> float_to_list(Value);
format_value(Value, double) when is_number(Value) -> float_to_list(Value);
format_value(Value, binary) when is_binary(Value) -> http_uri:encode(Value);
format_value(Value, string) when is_binary(Value) -> http_uri:encode(Value);
format_value(Value, atom) when is_atom(Value) -> http_uri:encode(atom_to_binary(Value, latin1));
format_value(Value, {list, Separator, ItemType}) when is_list(Value) -> lists:join(Separator, [ format_value(Item, ItemType) || Item <- Value ]).

%% Local functions

binary_to_float_or_int(B) ->
    try
        binary_to_float(B)
    catch
        error:badarg ->
            float(binary_to_integer(B))
    end.
