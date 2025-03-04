-module(igor_json).

%% Include files

%% Exported functions

-export([
    parse/4,
    parse/3,
    parse_value/2,
    pack/2,
    pack_value/2,
    maybe_pack_field/4,
    encode/1,
    decode/1
]).

-export_type([
    json/0,
    json_object/0,
    json_list/0,
    json_number/0,
    json_string/0,
    json_bool/0,
    json_null/0
]).

-export_type([
    pack_type/1,
    parse_type/1
]).

-type json_object() :: #{json_string() => json()}.
-type json_list() :: [json()].
-type json_number() :: number().
-type json_string() :: binary().
-type json_bool() :: boolean().
-type json_null() :: 'null'.
-type json() :: json_number() | json_string() | json_bool() | json_null() | json_list() | json_object().

-type parse_type(T) :: igor_types:primitive_type() | 'json' | {'list', parse_type(_Item)} |
    {'dict', parse_type(_Key), parse_type(_Value)} |
    {'option', parse_type(_Value)} | {'custom', fun((json()) -> T)}.

-type pack_type(T) :: igor_types:primitive_type() | 'json' | {'list', pack_type(_Item)} |
    {'dict', pack_type(_Key), pack_type(_Value)} |
    {'option', pack_type(_Value)} | {'custom', fun((T) -> json())}.

%% API

-spec parse(json_object(), Key :: json_string(), parse_type(T), Default :: T) -> T.

parse(JsonObject, Key, Type, Default) when is_map(JsonObject) ->
    case maps:get(Key, JsonObject, undefined) of
        undefined -> Default;
        null when element(1, Type) =:= nullable -> null;
        null -> Default;
        Value -> parse_value(Value, Type)
    end.

-spec parse(JsonObject, Key, Type) -> Value when
      JsonObject :: json_object(),
      Key :: json_string(),
      Type :: parse_type(T),
      Value :: T.

parse(JsonObject, Key, Type) when is_map(JsonObject) ->
    case maps:get(Key, JsonObject, undefined) of
        undefined -> error({require, Key});
        null when element(1, Type) =:= nullable -> null;
        null -> error({require, Key});
        Value -> parse_value(Value, Type)
    end.

-spec pack(T, pack_type(T)) -> json().

pack(undefined, _Type) -> null;
pack(null, _Type) -> null;
pack(Value, Type) -> pack_value(Value, Type).

-spec maybe_pack_field(json_object(), binary(), T, pack_type(T)) -> json_object().

maybe_pack_field(JsonObject, _Key, undefined, _Type) ->
    JsonObject;
maybe_pack_field(JsonObject, Key, Value, Type) ->
    Value1 = pack(Value, Type),
    maps:put(Key, Value1, JsonObject).

%% Local functions

parse_value(Value, boolean) when is_boolean(Value) -> Value;
parse_value(Value, sbyte) when is_integer(Value) -> Value;
parse_value(Value, byte) when is_integer(Value) -> Value;
parse_value(Value, short) when is_integer(Value) -> Value;
parse_value(Value, ushort) when is_integer(Value) -> Value;
parse_value(Value, int) when is_integer(Value) -> Value;
parse_value(Value, uint) when is_integer(Value) -> Value;
parse_value(Value, long) when is_integer(Value) -> Value;
parse_value(Value, ulong) when is_integer(Value) -> Value;
parse_value(Value, float) when is_number(Value) -> float(Value);
parse_value(Value, double) when is_number(Value) -> float(Value);
parse_value(Value, binary) when is_binary(Value) -> base64:decode(Value);
parse_value(Value, string) when is_binary(Value) -> Value;
parse_value(Value, atom) when is_binary(Value) -> binary_to_atom(Value, latin1);
parse_value(Value, json) -> Value;
parse_value(List, {list, Type}) when is_list(List) ->
    [ parse_value(Value, Type) || Value <- List  ];
parse_value(JsonObject, {dict, KeyType, ValueType}) ->
    [ {parse_key(Key, KeyType), parse_value(Value, ValueType)} || {Key, Value} <- maps:to_list(JsonObject) ];
parse_value(null, {option, _}) -> undefined;
parse_value(Value, {option, Type}) -> parse_value(Value, Type);
parse_value(null, {nullable, _}) -> null;
parse_value(Value, {nullable, Type}) -> parse_value(Value, Type);
parse_value(Value, {custom, Parser}) -> Parser(Value).

parse_key(<<"true">>, boolean) -> true;
parse_key(<<"false">>, boolean) -> false;
parse_key(Value, sbyte) -> list_to_integer(binary_to_list(Value));
parse_key(Value, byte) -> list_to_integer(binary_to_list(Value));
parse_key(Value, short) -> list_to_integer(binary_to_list(Value));
parse_key(Value, ushort) -> list_to_integer(binary_to_list(Value));
parse_key(Value, int) -> list_to_integer(binary_to_list(Value));
parse_key(Value, uint) -> list_to_integer(binary_to_list(Value));
parse_key(Value, long) -> list_to_integer(binary_to_list(Value));
parse_key(Value, ulong) -> list_to_integer(binary_to_list(Value));
parse_key(Value, float) -> list_to_float(binary_to_list(Value));
parse_key(Value, double) -> list_to_float(binary_to_list(Value));
parse_key(Value, binary) -> Value;
parse_key(Value, string) -> Value;
parse_key(Value, atom) -> binary_to_atom(Value, latin1);
parse_key(Value, {custom, Parser}) -> Parser(Value);
parse_key(_Value, Type) -> erlang:error({unsupported_json_key_type, Type}).

pack_value(Value, boolean) when is_boolean(Value) -> Value;
pack_value(Value, sbyte) when is_integer(Value) -> Value;
pack_value(Value, byte) when is_integer(Value) -> Value;
pack_value(Value, short) when is_integer(Value) -> Value;
pack_value(Value, ushort) when is_integer(Value) -> Value;
pack_value(Value, int) when is_integer(Value) -> Value;
pack_value(Value, uint) when is_integer(Value) -> Value;
pack_value(Value, long) when is_integer(Value) -> Value;
pack_value(Value, ulong) when is_integer(Value) -> Value;
pack_value(Value, float) when is_number(Value) -> Value;
pack_value(Value, double) when is_number(Value) -> Value;
pack_value(Value, binary) when is_binary(Value) -> base64:encode(Value);
pack_value(Value, string) when is_binary(Value) -> Value;
pack_value(Value, atom) when is_atom(Value) -> atom_to_binary(Value, latin1);
pack_value(Value, json) -> Value;
pack_value(List, {list, Type}) when is_list(List) ->
    [ pack_value(Value, Type) || Value <- List  ];
pack_value(Dict, {dict, KeyType, ValueType}) when is_list(Dict) ->
    maps:from_list([ {pack_key(Key, KeyType), pack_value(Value, ValueType)} || {Key, Value} <- Dict ]);
pack_value(undefined, {option, _}) -> null;
pack_value(null, {option, _}) -> null;
pack_value(Value, {option, Type}) -> pack_value(Value, Type);
pack_value(null, {nullable, _Type}) -> null;
pack_value(Value, {nullable, Type}) -> pack_value(Value, Type);
pack_value(Value, {custom, Packer}) -> Packer(Value).

pack_key(true, boolean) -> <<"true">>;
pack_key(false, boolean) -> <<"false">>;
pack_key(Value, sbyte) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
pack_key(Value, byte) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
pack_key(Value, short) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
pack_key(Value, ushort) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
pack_key(Value, int) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
pack_key(Value, uint) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
pack_key(Value, long) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
pack_key(Value, ulong) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
pack_key(Value, float) when is_number(Value) -> list_to_binary(float_to_list(Value));
pack_key(Value, double) when is_number(Value) -> list_to_binary(float_to_list(Value));
pack_key(Value, binary) when is_binary(Value) -> Value;
pack_key(Value, string) when is_binary(Value) -> Value;
pack_key(Value, atom) when is_atom(Value) -> atom_to_binary(Value, latin1);
pack_key(Value, {custom, Packer}) -> Packer(Value);
pack_key(_Value, Type) -> erlang:error({unsupported_json_key_type, Type}).

encode(Value) -> jsx:encode(Value).

decode(Json) -> jsx:decode(Json, [return_maps]).
