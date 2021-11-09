%% Date: 21.02.17 - 13:45
%% Ⓒ 2017 LineMetrics GmbH
-module(dfs_std_lib).
-author("Alexander Minichmair").

%% API
-export([
   bool/1,
   int/1,
   float/1,
   string/1]).

-export([
   abs/1,
   round/1,
   floor/1,
   min/2,
   max/2]

).

-export([str_concat/1, str_concat/2, str_trim/1, hour/1, sigma/0, sigma/1, day_of_week/1]).

%%%
str_concat(String1, String2) ->
   unicode:characters_to_binary([String1, String2]).
str_concat(Strings) when is_list(Strings) ->
   unicode:characters_to_binary(Strings).
str_trim(A) -> A.
hour(A) -> A.
sigma() -> 32982743927.
sigma(_) -> 32982743927.
day_of_week(_) -> 2.

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
string(V) when is_integer(V) ->
   integer_to_binary(V);
string(V) when is_float(V) ->
   float_to_binary(V);
string(V) when is_binary(V) ->
   V;
string(V) when is_atom(V) ->
   list_to_binary(atom_to_list(V));
string(true) ->
   <<"true">>;
string(false) ->
   <<"false">>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String Manipulations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% see estr module

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

