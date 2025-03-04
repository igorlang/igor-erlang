-define(sbyte(Var), Var:1/little-signed-integer-unit:8).
-define(byte(Var), Var:1/little-unsigned-integer-unit:8).
-define(short(Var), Var:2/little-signed-integer-unit:8).
-define(ushort(Var), Var:2/little-unsigned-integer-unit:8).
-define(int(Var), Var:4/little-signed-integer-unit:8).
-define(uint(Var), Var:4/little-unsigned-integer-unit:8).
-define(long(Var), Var:8/little-signed-integer-unit:8).
-define(ulong(Var), Var:8/little-unsigned-integer-unit:8).
-define(float(Var), Var:4/little-signed-float-unit:8).
-define(double(Var), Var:8/little-signed-float-unit:8).
-define(bit(Var), (if Var =:= undefined -> 0; true -> 1 end):1).

-define(list(List, Packer), << <<Packer(__X)>> || __X <- List >>).

-define(byte_list(List), ?list(List, ?byte)).
-define(short_list(List), ?list(List, ?short)).
-define(ushort_list(List), ?list(List, ?ushort)).
-define(int_list(List), ?list(List, ?int)).
-define(uint_list(List), ?list(List, ?uint)).
-define(long_list(List), ?list(List, ?long)).
-define(ulong_list(List), ?list(List, ?ulong)).
-define(float_list(List), ?list(List, ?float)).
-define(double_list(List), ?list(List, ?double)).
-define(bit_list(List), ?list(List, ?bit)).

-define(binary(Size,Binary), ?ushort(Size),Binary:Size/binary).
-define(wbinary(Binary), << <<?ushort(__Size),Binary/binary>> || __Size <- [byte_size(Binary)]>>/binary).
