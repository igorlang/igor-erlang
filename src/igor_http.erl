-module(igor_http).

%% Include files

-include("igor_http.hrl").
-include("igor.hrl").

%% Exported functions

-export([
    compose_query/1,
    parse_query/3,
    parse_query/4,
    parse_value/2,
    format_value/2,
    parse_header/3,
    parse_header/4
]).

%% API

compose_query(QueryParts) ->
    compose_query_string(lists:flatmap(
        fun({Name, Value}) -> [{Name, Value}];
           ({Name, Value, Format}) -> format_query(Name, Value, Format);
           ({_Name, Value, _Format, Default}) when Value =:= Default -> [];
           ({Name, Value, Format, _Default}) -> format_query(Name, Value, Format)
        end, QueryParts)).

compose_query_string(Parts) ->
    lists:join($&, [ [Name, $=, Value] || {Name, Value} <- Parts ]).

format_query(_Name, undefined, _) ->
    [];
format_query(Name, Value, {option, Type}) ->
    format_query(Name, Value, Type);
format_query(Name, Value, {list, ItemType}) when is_list(Value) ->
    [ {Name, format_value(Item, ItemType)} || Item <- Value ];
format_query(Name, Value, {list_index, ItemType}) when is_list(Value) ->
    ValueIndexed = lists:zip(lists:seq(0, length(Value) - 1), Value),
    [ {[Name, $[, integer_to_binary(I), $]], format_value(Item, ItemType)} || {I, Item} <- ValueIndexed ];
format_query(Name, Value, Type) ->
    [ {Name, format_value(Value, Type)} ].

parse_query(Param, Qs, {option, Type}) ->
    parse_query(Param, Qs, Type, undefined);
parse_query(Param, Qs, Type) ->
    try parse_query_opt(Param, Qs, Type) of
        undefined -> throw(#bad_request{reason = {missing_query_parameter, Param}});
        Value -> Value
    catch
        _:_ -> throw(#bad_request{reason = {malformed_query_parameter, Param}})
    end.

parse_query(Param, Qs, Type, Default) ->
    try parse_query_opt(Param, Qs, Type) of
        undefined -> Default;
        Value -> Value
    catch
        _:_ -> throw(#bad_request{reason = {malformed_query_parameter, Param}})
    end.

parse_query_opt(Param, Qs, {list, ItemType}) ->
    case proplists:get_all_values(Param, Qs) of
        [] -> undefined;
        Values -> [ parse_value(Value, ItemType) || Value <- Values ]
    end;
parse_query_opt(Param, Qs, {custom_query, Fun}) ->
    Fun(Param, Qs);
parse_query_opt(Param, Qs, {option, Type}) ->
    parse_query_opt(Param, Qs, Type);
parse_query_opt(Param, Qs, Type) ->
    case proplists:get_value(Param, Qs) of
        undefined -> undefined;
        Bin -> parse_value(Bin, Type)
    end.

parse_header(Header, Value, {option, Type}) ->
    parse_header(Header, Value, Type, undefined);
parse_header(Header, undefined, _Type) ->
    throw(#bad_request{reason = {missing_header, Header}});
parse_header(_Header, Value, Type) when is_binary(Value) ->
    igor_string:parse_value(Value, Type).

parse_header(_Header, undefined, _Type, Default) ->
    Default;
parse_header(Header, Value, Type, _Default) ->
    parse_header(Header, Value, Type).

parse_value(true, boolean) -> true;
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
parse_value(Bin, {custom, Fun}) -> Fun(Bin);
parse_value(Bin, {json, JsonTag}) -> igor_json:parse_value(igor_json:decode(Bin), JsonTag).

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
format_value(Value, {list, Separator, ItemType}) when is_list(Value) -> lists:join(Separator, [ format_value(Item, ItemType) || Item <- Value ]);
format_value(Value, {custom, Fun}) -> Fun(Value);
format_value(Value, {json, JsonTag}) -> http_uri:encode(igor_json:encode(igor_json:pack_value(Value, JsonTag))).

%% Local functions

binary_to_float_or_int(B) ->
    try
        binary_to_float(B)
    catch
        error:badarg ->
            float(binary_to_integer(B))
    end.
