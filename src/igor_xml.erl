-module(igor_xml).

%% Include files

-include_lib("xmerl/include/xmerl.hrl").

%% Exported functions

-export([
    encode/1,
    decode/1,
    decode_file/1,

    name/1,
    content/1,

    parse_element/2,
    parse_subelement/3,
    parse_subelement/4,
    parse_sequence_subelement/3,
    parse_sequence_subelement/4,
    parse_attribute/3,
    parse_attribute/4,
    parse_text/2,
    parse_text/3,
    parse_content/2,
    parse_content/3,
    parse_complex_type/2,
    parse_complex_type/3,
    parse_simple_type/2,

    pack_element/2,
    pack_subelement/3,
    pack_content/2,
    pack_complex_type/3,
    pack_simple_type/2
]).

-export_type([
    xml_element/0,
    xml_text/0,
    xml_content/0
]).

-type xml_element() :: #xmlElement{}.
-type xml_text() :: #xmlText{}.
-type xml_content() :: [#xmlElement{} | #xmlText{}].

-define(is_primitive_type(Type), Type =:= boolean; Type =:= sbyte; Type =:= byte; Type =:= short; Type =:= ushort; Type =:= int; Type =:= uint; Type =:= long; Type =:= ulong; Type =:= float; Type =:= double; Type =:= binary; Type =:= string; Type =:= atom).
-define(is_custom_simple_type(Type), element(1, Type) =:= custom_simple_type).
-define(is_simple_type(Type), ?is_custom_simple_type(Type); ?is_primitive_type(Type)).
-define(is_element(Type), element(1, Type) =:= custom_element; element(1, Type) =:= element).

%% API

encode(Xml) ->
    iolist_to_binary(xmerl:export_simple([Xml], xmerl_xml)).

decode(Binary) ->
    {Xml, _} = xmerl_scan:string(binary_to_list(Binary), [{comments, false}, {space, normalize}]),
    Xml.

decode_file(Path) ->
    case xmerl_scan:file(Path, [{comments, false}, {space, normalize}]) of
        {error, Error} -> error(Error);
        {Xml, []} -> Xml
    end.

name(#xmlElement{name = Name}) -> Name.

content(#xmlElement{content = Content}) ->
    [ C || C <- Content, not is_space(C) ].

is_space(#xmlText{value = " "}) -> true;
is_space(_) -> false.

parse_element(XmlElement, {custom_element, Fun}) ->
    Fun(XmlElement);
parse_element(XmlElement, {element, _Name, Type}) when ?is_element(Type) ->
    parse_element(XmlElement, Type);
parse_element(XmlElement, {element, Name, Type}) ->
    #xmlElement{name = Name} = XmlElement,
    parse_complex_type(XmlElement, Type);
parse_element(#xmlText{value = Value}, Type) when ?is_simple_type(Type) ->
    parse_simple_type(Value, Type).

parse_complex_type(XmlElement, {option, Type}) ->
    parse_complex_type(XmlElement,  Type, undefined);
parse_complex_type(XmlElement, Type) ->
    case parse_complex_type(XmlElement, Type, undefined) of
        undefined -> error({xml_content_required, XmlElement});
        Value -> Value
    end.

parse_complex_type(XmlElement, {option, Type}, Default) ->
    parse_complex_type(XmlElement, Type, Default);
parse_complex_type(XmlElement, {custom_complex_type, Fun}, Default) ->
    case Fun(XmlElement) of
        undefined -> Default;
        Val -> Val
    end;
parse_complex_type(XmlElement, {pair, KeyType, ValueType}, _Default) ->
    Key = parse_complex_type(XmlElement, KeyType),
    Value = parse_complex_type(XmlElement, ValueType),
    {Key, Value};
parse_complex_type(XmlElement, {attribute, Name, Type}, Default) ->
    parse_attribute(XmlElement, Name, Type, Default);
parse_complex_type(XmlElement, {subelement, Name, Type}, Default) ->
    parse_subelement(XmlElement, Name, Type, Default);
parse_complex_type(XmlElement, Type, Default) ->
    parse_content(content(XmlElement), Type, Default).

parse_subelement(XmlElement, ElementName, {option, Type}) ->
    parse_subelement(XmlElement, ElementName, Type, undefined);
parse_subelement(XmlElement, ElementName, Type) ->
    case parse_subelement(XmlElement, ElementName, Type, undefined) of
        undefined -> error({xml_element_required, ElementName, XmlElement});
        Value -> Value
    end.

parse_subelement(XmlElement, ElementName, {option, Type}, Default) ->
    parse_subelement(XmlElement, ElementName, Type, Default);
parse_subelement(XmlElement, ElementName, {repeated, Type}, Default) ->
    #xmlElement{content = Content} = XmlElement,
    case [ parse_complex_type(Subelement, Type) || #xmlElement{name = Name} = Subelement <- Content, Name =:= ElementName ] of
        [] -> Default;
        Value -> Value
    end;
parse_subelement(XmlElement, ElementName, Type, Default) ->
    #xmlElement{content = Content} = XmlElement,
    case lists:keyfind(ElementName, #xmlElement.name, Content) of
        false -> Default;
        Subelement -> parse_complex_type(Subelement, Type, Default)
    end.

parse_sequence_subelement(XmlContent, ElementName, {option, Type}) ->
    parse_sequence_subelement(XmlContent, ElementName, Type, undefined);
parse_sequence_subelement(XmlContent, ElementName, Type) ->
    case parse_sequence_subelement(XmlContent, ElementName, Type, undefined) of
        {undefined, Rest} -> error({xml_element_required, ElementName, Rest});
        {Value, Rest} -> {Value, Rest}
    end.

parse_sequence_subelement(XmlContent, ElementName, {option, Type}, Default) ->
    parse_sequence_subelement(XmlContent, ElementName, Type, Default);
parse_sequence_subelement(XmlContent, ElementName, {repeated, Type}, Default) ->
    case lists:splitwith(fun(#xmlElement{name = Name}) -> Name =:= ElementName; (_) -> false end, XmlContent) of
        {[], _} -> {Default, XmlContent};
        {List, Rest} -> {[ parse_complex_type(Subelement, Type) || Subelement <- List ], Rest}
    end;
parse_sequence_subelement([ #xmlElement{name = ElementName} = XmlElement | Rest ], ElementName, Type, Default) ->
    {parse_complex_type(XmlElement, Type, Default), Rest};
parse_sequence_subelement(XmlContent, _ElementName, _Type, Default) ->
    {Default, XmlContent}.

parse_attribute(XmlElement, AttributeName, {option, Type}) ->
    parse_attribute(XmlElement, AttributeName, Type, undefined);
parse_attribute(XmlElement, AttributeName, Type) ->
    case parse_attribute(XmlElement, AttributeName, Type, undefined) of
        undefined -> error({xml_attribute_required, AttributeName, XmlElement});
        Value -> Value
    end.

parse_attribute(XmlElement, AttributeName, {option, Type}, Default) ->
    parse_attribute(XmlElement, AttributeName, Type, Default);
parse_attribute(XmlElement, AttributeName, {repeated, Type}, Default) ->
    #xmlElement{attributes = Attributes} = XmlElement,
    case [ parse_simple_type(Value, Type) || #xmlAttribute{name = Name, value = Value} <- Attributes, Name =:= AttributeName ] of
        [] -> Default;
        Value -> Value
    end;
parse_attribute(XmlElement, AttributeName, Type, Default) ->
    #xmlElement{attributes = Attributes} = XmlElement,
    case lists:keyfind(AttributeName, #xmlAttribute.name, Attributes) of
        #xmlAttribute{value = Value} -> parse_simple_type(Value, Type);
        false -> Default
    end.

parse_text(XmlElement, {option, Type}) ->
    parse_text(XmlElement, Type, undefined);
parse_text(XmlElement, Type) ->
    case parse_text(XmlElement, Type, undefined) of
        undefined -> error({xml_invalid_content_type, Type, XmlElement});
        Value -> Value
    end.

parse_text(XmlElement, {option, Type}, Default) ->
    parse_text(XmlElement, Type, Default);
parse_text(XmlElement, Type, Default) when ?is_simple_type(Type) ->
    #xmlElement{content = Content} = XmlElement,
    case Content of
        [#xmlText{value = Value}] -> parse_simple_type(Value, Type);
        [] -> Default;
        _ -> error({xml_invalid_content_type, Type, XmlElement})
    end.

parse_content(XmlContent, {option, Type}) ->
    parse_content(XmlContent, Type, undefined);
parse_content(XmlContent, Type) ->
    case parse_content(XmlContent, Type, undefined) of
        undefined -> error({xml_invalid_content_type, Type, XmlContent});
        Value -> Value
    end.

parse_content(XmlContent, {option, Type}, Default) ->
    parse_content(XmlContent, Type, Default);
parse_content(XmlContent, {custom_content, Fun}, _Default) ->
    Fun(XmlContent);
parse_content(XmlContent, Type, Default) when ?is_element(Type) ->
    case XmlContent of
        [] -> Default;
        [Subelement] -> parse_element(Subelement, Type);
        [_|_] -> error({xml_invalid_content_type, Type, XmlContent})
    end;
parse_content(XmlContent, {repeated, Type}, _Default) ->
    [ parse_complex_type(Item, Type) || Item <- XmlContent ];
parse_content(XmlContent, {list, Type}, _Default) ->
    [ parse_element(Item, Type) || Item <- XmlContent ];
parse_content(XmlContent, {kvlist, KeyType, ValueType}, _Default) ->
    parse_kvlist(XmlContent, KeyType, ValueType, []);
parse_content(XmlContent, Type, Default) when ?is_simple_type(Type) ->
    case XmlContent of
        [#xmlText{value = Value}] -> parse_simple_type(Value, Type);
        [] -> Default;
        _ -> error({xml_invalid_content_type, Type, XmlContent})
    end.

parse_kvlist([Key, Value | Tail], KeyType, ValueType, Acc) ->
    Acc1 = [{parse_element(Key, KeyType), parse_element(Value, ValueType)} | Acc],
    parse_kvlist(Tail, KeyType, ValueType, Acc1);
parse_kvlist([], _, _, Acc) ->
    lists:reverse(Acc).

pack_complex_type(ElementName, undefined, Type) ->
    error({xml_invalid_value, ElementName, Type});
pack_complex_type(ElementName, List, {list, Type}) ->
    #xmlElement{name = ElementName, content = [S || Item <- List, S <- pack_content(Item, Type) ]};
pack_complex_type(ElementName, Value, {custom_complex_type, Fun}) ->
    Fun(Value, ElementName);
pack_complex_type(ElementName, {Key, Value}, {pair, KeyType, ValueType}) ->
    #xmlElement{attributes = A1, content = C1} = pack_complex_type(ElementName, Key, KeyType),
    #xmlElement{attributes = A2, content = C2} = pack_complex_type(ElementName, Value, ValueType),
    #xmlElement{name = ElementName, attributes = A1 ++ A2, content = C1 ++ C2};
pack_complex_type(ElementName, Value, {attribute, Name, ValueType}) ->
    #xmlElement{name = ElementName, attributes = [#xmlAttribute{name = Name, value = pack_simple_type(Value, ValueType)}]};
pack_complex_type(ElementName, Value, {subelement, Name, ValueType}) ->
    #xmlElement{name = ElementName, content = pack_subelement(Name, Value, ValueType)};
pack_complex_type(ElementName, Value, Type) when ?is_simple_type(Type) ->
    #xmlElement{name = ElementName, content = [#xmlText{value = pack_simple_type(Value, Type)}]};
pack_complex_type(ElementName, Value, Type) ->
    #xmlElement{name = ElementName, content = pack_content(Value, Type)}.

pack_subelement(_ElementName, undefined, {option, _Type}) ->
    [];
pack_subelement(ElementName, Value, {option, Type}) ->
    pack_subelement(ElementName, Value, Type);
pack_subelement(ElementName, undefined, Type) ->
    error({xml_invalid_value, ElementName, Type});
pack_subelement(ElementName, List, {repeated, Type}) ->
    [ S || Item <- List, S <- pack_subelement(ElementName, Item, Type) ];
pack_subelement(ElementName, Value, Type) ->
    [ pack_complex_type(ElementName, Value, Type) ].

pack_element(Value, {custom_element, Fun}) ->
    Fun(Value);
pack_element(Value, {element, _ElementName, Type}) when ?is_element(Type) ->
    pack_element(Value, Type);
pack_element(Value, {element, ElementName, Type}) ->
    pack_complex_type(ElementName, Value, Type).

pack_content(undefined, {option, _Type}) ->
    [];
pack_content(Value, {option, Type}) ->
    pack_content(Value, Type);
pack_content(undefined, Type) ->
    error({xml_invalid_content, Type});
pack_content(Value, Type) when ?is_element(Type) ->
    [ pack_element(Value, Type) ];
pack_content(Value, {custom_content, Fun}) ->
    Fun(Value);
pack_content(Value, {list, Type}) ->
    [ C || Item <- Value, C <- pack_content(Item, Type) ];
pack_content(List, {kvlist, KeyType, ValueType}) ->
    [ Item || {Key, Value} <- List, Item <- [ pack_element(Key, KeyType), pack_element(Value, ValueType) ] ].

%% Local functions

parse_bool("1") -> true;
parse_bool("0") -> false;
parse_bool("true") -> true;
parse_bool("false") -> false;
parse_bool("True") -> true;
parse_bool("False") -> false.

parse_int(Value) ->
    list_to_integer(Value).

parse_float(Value) ->
    try
        list_to_float(Value)
    catch
        error:badarg ->
            float(list_to_integer(Value))
    end.

parse_simple_type(Value, boolean) -> parse_bool(Value);
parse_simple_type(Value, sbyte) -> parse_int(Value);
parse_simple_type(Value, byte) -> parse_int(Value);
parse_simple_type(Value, short) -> parse_int(Value);
parse_simple_type(Value, ushort) -> parse_int(Value);
parse_simple_type(Value, int) -> parse_int(Value);
parse_simple_type(Value, uint) -> parse_int(Value);
parse_simple_type(Value, long) -> parse_int(Value);
parse_simple_type(Value, ulong) -> parse_int(Value);
parse_simple_type(Value, float) -> parse_float(Value);
parse_simple_type(Value, double) -> parse_float(Value);
parse_simple_type(Value, binary) -> base64:decode(Value);
parse_simple_type(Value, string) -> iolist_to_binary(Value);
parse_simple_type(Value, atom) -> list_to_atom(Value);
parse_simple_type(Value, {custom_simple_type, Fun}) -> Fun(Value).

pack_simple_type(true, boolean) -> "true";
pack_simple_type(false, boolean) -> "false";
pack_simple_type(true, {boolean, True, _}) -> True;
pack_simple_type(false, {boolean, _, False}) -> False;
pack_simple_type(Value, sbyte) -> integer_to_list(Value);
pack_simple_type(Value, byte) -> integer_to_list(Value);
pack_simple_type(Value, short) -> integer_to_list(Value);
pack_simple_type(Value, ushort) -> integer_to_list(Value);
pack_simple_type(Value, int) -> integer_to_list(Value);
pack_simple_type(Value, uint) -> integer_to_list(Value);
pack_simple_type(Value, long) -> integer_to_list(Value);
pack_simple_type(Value, ulong) -> integer_to_list(Value);
pack_simple_type(Value, float) -> float_to_list(Value);
pack_simple_type(Value, double) -> float_to_list(Value);
pack_simple_type(Value, binary) -> base64:encode(Value);
pack_simple_type(Value, string) -> binary_to_list(Value);
pack_simple_type(Value, atom) -> atom_to_list(Value);
pack_simple_type(Value, {custom_simple_type, Fun}) -> Fun(Value).
