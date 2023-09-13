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
   list_of_strings/1,
   member/2,
   not_member/2,
   size/1,
   list_join/1,
   nth/2,
   list_unique/1,

   list_concat/1,
   list_concat/2,

   list_append/2,
   list_delete/2,
   list_last/1,
   list_max/1,
   list_min/1,
   nthtail/2,
   list_reverse/1,
   list_sort/1,
   sublist/2,
   sublist/3,
   list_subtract/2,
   list_sum/1,
   list_usort/1]).


-export([
   min/2,
   max/2,
   head/1]).

-export([
   crc32/1,
   phash/1,
   base64_encode/1,
   base64_decode/1,
   bytesize/1]).


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% byte size
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bytesize(String) when is_binary(String) ->
   byte_size(String).

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
   lists:uniq(L);
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

%% Returns a copy of List1 where the first element matching Elem is deleted, if there is such an element.
list_delete(Elem, List) when is_list(List) ->
   lists:delete(Elem, List);
list_delete(_Elem, _NotList) ->
   throw("list_delete/2: param is not a list!").

% Returns the last element in List.
list_last(List) when is_list(List) ->
   lists:last(List);
list_last(_NotList) ->
   throw("list_last/1: param is not a list!").

% Returns the first element of List that compares greater than or equal to all other elements of List.
list_max(List) when is_list(List) ->
   lists:max(List);
list_max(_NotList) ->
   throw("list_max/1: param is not a list!").

% Returns the first element of List that compares less than or equal to all other elements of List.
list_min(List) when is_list(List) ->
   lists:min(List);
list_min(_NotList) ->
   throw("list_min/1: param is not a list!").

% Returns the Nth tail of List, that is, the sublist of List starting at N+1 and continuing up to the end of the list.
nthtail(N, List) when is_list(List) ->
   lists:nthtail(N, List);
nthtail(_N, _NotList) ->
   throw("nthtail/2: param is not a list!").

% Returns a list with the elements in List1 in reverse order.
list_reverse(List) when is_list(List) ->
   lists:reverse(List);
list_reverse(_NotList) ->
   throw("list_reverse/1: param is not a list!").

% Returns a list containing the sorted elements of List1.
list_sort(List) when is_list(List) ->
   lists:sort(List);
list_sort(_NotList) ->
   throw("list_sort/1: param is not a list!").

% Returns the sublist of List1 starting at position 1 and with (maximum) Len elements.
% It is not an error for Len to exceed the length of the list, in that case the whole list is returned.
sublist(List, Len) when is_list(List) ->
   lists:sublist(List, Len);
sublist(_List, _Len) ->
   throw("sublist/2: param is not a list!").

% Returns the sublist of List1 starting at Start and with (maximum) Len elements.
% It is not an error for Start+Len to exceed the length of the list.
sublist(List, Start, Len) when is_list(List) ->
   lists:sublist(List, Start, Len);
sublist(_List, _Start, _Len) ->
   throw("sublist/3: param is not a list!").

% Returns a new list List3 that is a copy of List1, subjected to the following procedure:
% for each element in List2, its first occurrence in List1 is deleted.
list_subtract(L1, L2) when is_list(L1), is_list(L2) ->
   lists:subtract(L1, L2);
list_subtract(_L1, _L2) ->
   throw("list_subtract/2: at least one of the params is not of type list!").

% Returns the sum of the elements in List.
list_sum(List) when is_list(List) ->
   lists:sum(List);
list_sum(_List) ->
   throw("list_sum/1: param is not a list!").

% Returns a list containing the sorted elements of List1 where all
% except the first element of the elements comparing equal have been deleted.
-spec list_usort(List :: list()) -> list().
list_usort(List) when is_list(List) ->
   lists:usort(List);
list_usort(_NotList) ->
   throw("list_usort/1: param is not a list!").


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



