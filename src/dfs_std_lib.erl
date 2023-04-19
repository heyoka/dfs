%% Date: 21.02.17 - 13:45
%% Ⓒ 2017 heyoka
-module(dfs_std_lib).
-author("Alexander Minichmair").

%% API
-export([
   bool/1,
   int/1,
   float/1,
   string/1, is_string/1]).

-export([
   list_join/2,
   list_of_strings/1
   , member/2,
   not_member/2,
   size/1,
   list_join/1,
   nth/2,
   list_unique/1,

   list_concat/1,
   list_concat/2,

   list_append/2]).


-export([
   min/2,
   max/2,
   head/1]).

-export([
   crc32/1,
   phash/1, base64_encode/1, base64_decode/1]).


-export([test_fun/1, test_fun/2]).

test_fun(_Any, _Other) ->
   2.
test_fun(_Any) ->
   1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data Type Conversions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc type conversion to a boolean value
%%
%% any number less than zero : 'false',
%% any empty binary or binary-string : 'false',
%% the atom false: 'false',
%% anything else : 'true' 
%%
-spec bool(any()) -> true|false.
bool(V) when is_integer(V) orelse is_float(V) ->
   V > 0;
bool(<<>>) -> false;
bool(V) when is_binary(V) ->
   case binary_to_number(V) of
      false -> byte_size(V) /= 0;
      V1    -> bool(V1)
   end;
bool(false) -> false;
bool(V) when is_atom(V) ->
   true.

%% @doc type conversion to an integer value
%%
%% the atom true : 1,
%% the atom false: 0,
%% any binary that can not be converted to a number value : 0
-spec int(any()) -> integer().
int(V) when is_binary(V) ->
   case binary_to_number(V) of
      false -> 0;
      Value -> erlang:trunc(Value)
   end;
int(V) when is_float(V) ->
   erlang:trunc(V);
int(V) when is_integer(V) ->
   V;
int(true) ->
   1;
int(_) ->
   0.

%% @doc type conversion to a float value
%%
%% the atom true : 1.0,
%% the atom false: 0.0,
%% any binary that can not be converted to a number value : 0
-spec float(any()) -> float().
float(V) when is_binary(V) ->
   case binary_to_number(V) of
      false -> 0.0;
      Value -> erlang:float(Value)
   end;
float(true) ->
   1.0;
float(false) ->
   0.0;
float(V) when is_number(V) ->
   erlang:float(V);
float(_) ->
   0.0.

%% @doc type conversion to a binary-string value
%%
-spec string(any()) -> binary().
string(V) when is_binary(V) ->
   V;
string(V) when is_integer(V) ->
   integer_to_binary(V);
string(V) when is_float(V) ->
   float_to_binary(V, [{decimals, 8}, compact]);
string(V) when is_atom(V) ->
   list_to_binary(atom_to_list(V));
string(true) ->
   <<"true">>;
string(false) ->
   <<"false">>;
string(L) when is_list(L) ->
   list_join(<<",">>, list_of_strings(L)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPE check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% type checks is_x will be used from the erlang module except for the following:
is_string(BinString) -> erlang:is_binary(BinString).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String Manipulations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% %% see estr module for str_ functions
%%

%% %%%%%%%%%%%%%%%  string-lists
-spec list_join(list()) -> string().
list_join(L) when is_list(L) ->
   list_join(<<",">>, L).

-spec list_join(binary(), list()) -> string().
list_join(Sep, L) when is_list(L) ->
   erlang:iolist_to_binary(lists:join(Sep, L)).

%% convert every entry of a list to a string and return the list of strings
list_of_strings(L) when is_list(L) ->
   [string(S) || S <- L].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% maps and lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
head([H|_L]) -> H.
nth(N, L) when is_list(L), is_integer(N), N > 0 -> lists:nth(N, L);
nth(N, _L) when not is_integer(N) orelse N < 1 -> throw("nth/2: first param must be an integer > 0!");
nth(_N, _L) -> throw("nth/2: second param is not a list!").

list_unique(L) when is_list(L) ->
   sets:to_list(sets:from_list(L));
list_unique(_) ->
   throw("list_unique/1: param is not a list!").

list_concat(List) when is_list(List) ->
   lists:concat(List);
list_concat(_NotList) ->
   throw("list_concat/1: param is not a list!").

%% deprecated, since the usage is misleading in dfs - use list_append instead
list_concat(L1, L2) when is_list(L1), is_list(L2) ->
   lists:concat([L1, L2]);
list_concat(_L1, _L2) ->
   throw("list_concat/2: one or all params not type list!").

list_append(L1, L2) when is_list(L1), is_list(L2) ->
   lists:append([L1, L2]);
list_append(_L1, _L2) ->
   throw("list_append/2: one or all params not type list!").

-spec member(binary()|number(), list()|map()) -> true|false.
member(Ele, List) when is_list(List) -> lists:member(Ele, List);
member(Ele, Map) when is_map(Map) andalso is_map_key(Ele, Map) -> true;
member(_Ele, _) -> false.
-spec not_member(binary()|number(), list()|map()) -> true|false.
not_member(Ele, Coll) -> not member(Ele, Coll).


-spec size(map()|list()) -> integer().
size(Map) when is_map(Map) ->
   maps:size(Map);
size(List) when is_list(List) ->
   length(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% other number handling or math functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min(Val1, Val2) ->
   erlang:min(Val1, Val2).
max(Val1, Val2) ->
   erlang:max(Val1, Val2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% hashing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crc32(Val) ->
   erlang:crc32(Val).

phash(Val) ->
   erlang:phash2(Val).

base64_encode(Data) when is_binary(Data) ->
   base64:encode(Data);
base64_encode(_) ->
   throw("base64_encode/1: parameter must be of type string!").

base64_decode(Data) when is_binary(Data)->
   base64:decode(Data);
base64_decode(_) ->
   throw("base64_decode/1: parameter must be of type string!").



%%%%%%%%%%%%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%
binary_to_number(L) when is_binary(L) ->
   Float = (catch erlang:binary_to_float(L)),
   case is_number(Float) of
      true -> Float;
      false -> Int = (catch erlang:binary_to_integer(L)),
         case is_number(Int) of
            true -> Int;
            false -> false
         end
   end.



