-module(igor_binary).

%% -compile(inline).

%% Include files

-include("igor_binary.hrl").

%% Exported functions

-export([
    pack_value/2,
    maybe_pack_value/2,

    parse_value/2,
    maybe_parse_value/3,

    parse_varint/1,
    pack_varint/1,
    parse_signed/1,
    pack_signed/1
]).

-export_type([
    parse_type/1,
    pack_type/1
]).

-type parse_type(T) :: igor_types:primitive_type() | {'list', parse_type(_Item)} |
    {'flags', parse_type(_Enum)} |
    {'dict', parse_type(_Key), parse_type(_Value)} |
    {'pair', parse_type(_Key), parse_type(_Value)} |
    {'option', parse_type(_Value)} | {'custom', fun((binary()) -> T)} |
    {'enum', igor_types:integer_type(), fun((integer()) -> T)}.

-type pack_type(T) :: igor_types:primitive_type() | {'list', pack_type(_Item)} |
    {'flags', pack_type(_Enum)} |
    {'dict', pack_type(_Key), pack_type(_Value)} |
    {'pair', pack_type(_Key), pack_type(_Value)} |
    {'option', pack_type(_Value)} | {'custom', fun((T) -> iodata())} |
    {'enum', igor_types:integer_type(), fun((T) -> integer())}.

-spec parse_value(binary(), parse_type(T)) -> {T, binary()}.

%% API

parse_value(<<?byte(Value),Tail/binary>>, boolean) -> {Value =/= 0, Tail};
parse_value(<<?sbyte(Value),Tail/binary>>, sbyte) -> {Value, Tail};
parse_value(<<?byte(Value),Tail/binary>>, byte) -> {Value, Tail};
parse_value(<<?short(Value),Tail/binary>>, short) -> {Value, Tail};
parse_value(<<?ushort(Value),Tail/binary>>, ushort) -> {Value, Tail};
parse_value(<<?int(Value),Tail/binary>>, int) -> {Value, Tail};
parse_value(<<?uint(Value),Tail/binary>>, uint) -> {Value, Tail};
parse_value(<<?long(Value),Tail/binary>>, long) -> {Value, Tail};
parse_value(<<?ulong(Value),Tail/binary>>, ulong) -> {Value, Tail};
parse_value(<<?float(Value),Tail/binary>>, float) -> {Value, Tail};
parse_value(<<?double(Value),Tail/binary>>, double) -> {Value, Tail};
parse_value(Binary, binary) ->
    {Size, Tail} = parse_size(Binary),
    <<Value:Size/binary,Tail2/binary>> = Tail,
    {Value, Tail2};
parse_value(Binary, string) ->
    {Size, Tail} = parse_size(Binary),
    <<Value:Size/binary,Tail2/binary>> = Tail,
    {Value, Tail2};
parse_value(Binary, atom) ->
    {Size, Tail} = parse_size(Binary),
    <<Value:Size/binary,Tail2/binary>> = Tail,
    {binary_to_atom(Value, latin1), Tail2};
parse_value(Binary, {list, Type}) ->
    {Size, Tail} = parse_size(Binary),
    parse_list(Tail, Size, Type);
parse_value(Binary, {flags, {enum, IntType, EnumParser}}) ->
    {IntVal, Tail} = parse_value(Binary, IntType),
    {parse_flags(IntVal, EnumParser), Tail};
parse_value(Binary, {pair, KeyType, ValueType}) ->
    {Key, Tail1} = parse_value(Binary, KeyType),
    {Value, Tail2} = parse_value(Tail1, ValueType),
    {{Key,Value}, Tail2};
parse_value(Binary, {dict, KeyType, ValueType}) ->
    parse_value(Binary, {list, {pair, KeyType, ValueType}});
parse_value(<<?byte(Tag),Tail/binary>>, {union, Types}) ->
    parse_value(Tail, lists:nth(Tag + 1, Types));
parse_value(<<?byte(0),Tail/binary>>, {option, _Type}) ->
    {undefined, Tail};
parse_value(<<?byte(_),Tail/binary>>, {option, Type}) ->
    parse_value(Tail, Type);
parse_value(<<?sbyte(Value),Tail/binary>>, {enum, sbyte, EnumParser}) -> {EnumParser(Value), Tail};
parse_value(<<?byte(Value),Tail/binary>>, {enum, byte, EnumParser}) -> {EnumParser(Value), Tail};
parse_value(<<?short(Value),Tail/binary>>, {enum, short, EnumParser}) -> {EnumParser(Value), Tail};
parse_value(<<?ushort(Value),Tail/binary>>, {enum, ushort, EnumParser}) -> {EnumParser(Value), Tail};
parse_value(<<?int(Value),Tail/binary>>, {enum, int, EnumParser}) -> {EnumParser(Value), Tail};
parse_value(<<?uint(Value),Tail/binary>>, {enum, uint, EnumParser}) -> {EnumParser(Value), Tail};
parse_value(<<?long(Value),Tail/binary>>, {enum, long, EnumParser}) -> {EnumParser(Value), Tail};
parse_value(<<?ulong(Value),Tail/binary>>, {enum, ulong, EnumParser}) -> {EnumParser(Value), Tail};
parse_value(Binary, {custom, Parser}) -> Parser(Binary).

parse_size(Binary) ->
    parse_varint(Binary).

parse_list(Binary, Size, Tag) ->
    parse_list(Binary, Size, Tag, []).

parse_list(Binary, 0, _Tag, Result) ->
    {lists:reverse(Result), Binary};
parse_list(Binary, Size, Tag, Result) ->
    {Value, Tail} = parse_value(Binary, Tag),
    parse_list(Tail, Size-1, Tag, [Value|Result]).

-spec maybe_parse_value(Bit, Binary, Type) -> {Value, Tail} when
      Bit :: 0 | 1,
      Binary :: binary(),
      Type :: parse_type(T),
      Value :: T,
      Tail :: binary().

maybe_parse_value(0, Binary, _Tag) ->
    {undefined, Binary};
maybe_parse_value(1, Binary, Tag) ->
    parse_value(Binary, Tag).

%% Pack

-spec maybe_pack_value(T | 'undefined', pack_type(T)) -> iodata().

maybe_pack_value(undefined, _) -> <<>>;
maybe_pack_value(Value, Tag) -> pack_value(Value, Tag).

-spec pack_value(T, pack_type(T)) -> iodata().

pack_value(true, boolean) -> <<?byte(1)>>;
pack_value(false, boolean) -> <<?byte(0)>>;
pack_value(Value, sbyte) when is_integer(Value) -> <<?sbyte(Value)>>;
pack_value(Value, byte) when is_integer(Value) -> <<?byte(Value)>>;
pack_value(Value, short) when is_integer(Value) -> <<?short(Value)>>;
pack_value(Value, ushort) when is_integer(Value) -> <<?ushort(Value)>>;
pack_value(Value, int) when is_integer(Value) -> <<?int(Value)>>;
pack_value(Value, uint) when is_integer(Value) -> <<?uint(Value)>>;
pack_value(Value, long) when is_integer(Value) -> <<?long(Value)>>;
pack_value(Value, ulong) when is_integer(Value) -> <<?ulong(Value)>>;
pack_value(Value, float) when is_number(Value) -> <<?float(Value)>>;
pack_value(Value, double) when is_number(Value) -> <<?double(Value)>>;
pack_value(Value, binary) when is_binary(Value) -> [pack_size(byte_size(Value)), Value];
pack_value(Value, string) when is_binary(Value) -> [pack_size(byte_size(Value)), Value];
pack_value(Value, atom) when is_atom(Value) -> pack_value(atom_to_binary(Value, latin1), binary);
pack_value(List, {list, Type}) when is_list(List) -> [pack_size(length(List)), pack_list(List, Type)];
pack_value(List, {flags, {enum, IntType, EnumPacker}}) -> pack_value(pack_flags(List, EnumPacker), IntType);
pack_value({Key, Value}, {pair, KeyType, ValueType}) -> [pack_value(Key, KeyType), pack_value(Value, ValueType)];
pack_value(List, {dict, KeyType, ValueType}) when is_list(List) -> pack_value(List, {list, {pair, KeyType, ValueType}});
pack_value(Value, {union, Types}) -> pack_union(Value, Types);
pack_value(Map, {dict, KeyType, ValueType}) when is_map(Map) -> pack_value(maps:to_list(Map), {list, {pair, KeyType, ValueType}});
pack_value(undefined, {option, _}) -> <<?byte(0)>>;
pack_value(Value, {option, Type}) -> [<<?byte(1)>>, pack_value(Value, Type)];
pack_value(Value, {enum, IntType, EnumPacker}) -> pack_value(EnumPacker(Value), IntType);
pack_value(Value, {custom, Packer}) -> Packer(Value);
pack_value(Value, Type) -> erlang:error({badpack, Value, Type}).

pack_union(Value, [{Guard, Type}|Tail]) ->
    case Guard(Value) of
        true -> pack_value(Value, Type);
        false -> pack_union(Value, Tail)
    end;
pack_union(Value, []) ->
    erlang:error({badpack, Value, union}).

pack_size(Size) ->
    pack_varint(Size).

pack_list(List, byte) when is_list(List) -> ?byte_list(List);
pack_list(List, short) when is_list(List) -> ?short_list(List);
pack_list(List, ushort) when is_list(List) -> ?ushort_list(List);
pack_list(List, int) when is_list(List) -> ?int_list(List);
pack_list(List, uint) when is_list(List) -> ?uint_list(List);
pack_list(List, long) when is_list(List) -> ?long_list(List);
pack_list(List, ulong) when is_list(List) -> ?ulong_list(List);
pack_list(List, float) when is_list(List) -> ?float_list(List);
pack_list(List, double) when is_list(List) -> ?double_list(List);
pack_list(List, Type) when is_list(List) -> [ pack_value(Item, Type) || Item <- List ].

%% Signed Varint encoding

parse_signed(Binary) ->
    {N, Tail} = parse_varint(Binary),
    Res = parse_zigzag(N),
    {Res, Tail}.

pack_signed(Signed) ->
    N = pack_zigzag(Signed),
    pack_varint(N).

%% Varint encoding

parse_varint(Binary) -> parse_vi(Binary, 0, 0).

parse_vi(<<1:1, X:7, Rest/binary>>, N, Acc) -> parse_vi(Rest, N+1, X bsl (N*7) + Acc);
parse_vi(<<0:1, X:7, Rest/binary>>, N, Acc) -> {X bsl (N*7) + Acc, Rest};
parse_vi(_Binary, _N, _Acc) -> erlang:error(badvarint).

pack_varint(N) -> pack_vi(N).

pack_vi(N) when N =< 127 -> <<N>>;
pack_vi(N) -> <<1:1, (N band 127):7, (pack_vi(N bsr 7))/binary>>.

%% ZigZag encoding

parse_zigzag(N) when N band 1 =:= 0 -> N bsr 1;        %% N is even
parse_zigzag(N) -> -((N+1) bsr 1).                     %% N is odd

pack_zigzag(N) when N >= 0 -> N bsl 1;
pack_zigzag(N) -> ((-N) bsl 1) - 1.

%% Flags encoding

parse_flags(Val, EnumParser) ->
    parse_flags(Val, EnumParser, 1, []).

parse_flags(0, _EnumParser, _N, Acc) ->
    lists:reverse(Acc);
parse_flags(Val, EnumParser, N, Acc) ->
    Acc1 =
        if
            Val band 1 =:= 0 -> Acc;
            true -> [EnumParser(N) | Acc]
        end,
    parse_flags(Val bsr 1, EnumParser, N bsl 1, Acc1).

pack_flags(List, EnumPacker) ->
    lists:foldl(fun(V, Acc) -> Acc bor EnumPacker(V) end, 0, List).
