%% Date: 21.02.17 - 13:45
%% Ⓒ 2017 heyoka
-module(dfs_std_lib).
-author("Alexander Minichmair").

%% API
-export([
   bool/1,
   int/1,
   float/1,
   string/1]).

-export([
   list_join/2,
   list_of_strings/1
   , member/2,
   not_member/2,
   size/1,
   list_join/1,
   nth/2]).


-export([
   abs/1,
   round/1,
   floor/1,
   min/2,
   max/2, head/1]).

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
   float_to_binary(V);
string(V) when is_atom(V) ->
   list_to_binary(atom_to_list(V));
string(true) ->
   <<"true">>;
string(false) ->
   <<"false">>;
string(L) when is_list(L) ->
   list_join(<<",">>, list_of_strings(L)).


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
nth(N, L) when is_list(L) -> lists:nth(N, L).

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
abs(Val) ->
   erlang:abs(Val).
round(Val) when is_number(Val) ->
   erlang:round(Val).
floor(Val) when is_number(Val) ->
   erlang:trunc(Val).
min(Val1, Val2) ->
   erlang:min(Val1, Val2).
max(Val1, Val2) ->
   erlang:max(Val1, Val2).










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



