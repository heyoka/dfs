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
   str_contains/2,
   str_contains_any/2,
   str_pefix/2,
   str_suffix/2,
   str_length/1,
   str_index/2,
   str_last_index/2,
   str_replace/3,
   str_replace_all/3,
   str_substr/3,
   str_to_lower/1,
   str_to_upper/1,
   str_trim/1,
   str_ltrim/1,
   str_rtrim/1,
   str_strip/2,
   str_lstrip/2,
   str_rstrip/2,
   regex_replace/3]).

-export([
   abs/1,
   round/1,
   floor/1,
   min/2,
   max/2]

).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data Type Conversions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bool(V) when is_integer(V) orelse is_float(V) ->
   V > 0;
bool(V) when is_list(V) ->
   case string_to_number(V) of
      false -> false;
      V1    -> bool(V1)
   end;
bool(V) ->
   V.

int(V) when is_list(V) ->
   case string_to_number(V) of
      false -> 0;
      Value -> erlang:trunc(Value)
   end;
int(V) when is_float(V) ->
   erlang:trunc(V);
int(V) when is_integer(V) ->
   V;
int(true) ->
   1;
int(false) ->
   0.

float(V) when is_list(V) ->
   case string_to_number(V) of
      false -> 0.0;
      Value -> Value
   end;
float(true) ->
   1.0;
float(false) ->
   0.0;
float(V) when is_number(V) ->
   erlang:float(V);
float(V) ->
   V.

string(V) when is_integer(V) ->
   integer_to_list(V);
string(V) when is_float(V) ->
   float_to_list(V);
string(V) when is_list(V) ->
   V;
string(true) ->
   "true";
string(false) ->
   "false";
string(V) ->
   V.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String Manipulations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

str_contains(String, Search) when is_list(String) andalso is_list(Search) ->
   case string:str(String, Search) of
      0 -> false;
      _ -> true
   end.

str_contains_any(String, Search) ->
   lists:any(fun(E) -> str_contains(String, E) end, Search).

str_pefix(Prefix, String) -> lists:prefix(Prefix, String).

str_suffix(Suffix, String) -> lists:suffix(Suffix, String).

str_length(String) -> string:len(String).

str_index(String, SubString) -> string:str(String, SubString).

str_last_index(String, SubString) -> string:rstr(String, SubString).

str_replace(String, Search, Replace) when is_list(String) ->
   RBin = binary:replace(list_to_binary(String), list_to_binary(Search), list_to_binary(Replace)),
   binary_to_list(RBin).
str_replace_all(String, Search, Replace) when is_list(String) ->
   RBin = binary:replace(list_to_binary(String), list_to_binary(Search), list_to_binary(Replace), [global]),
   binary_to_list(RBin).

str_substr(String, From, To) ->
   string:sub_string(String, From, To).

str_to_lower(String) ->
   string:to_lower(String).

str_to_upper(String) ->
   string:to_upper(String).

%% remove whitespace
str_trim(String) -> string:strip(String).
str_ltrim(String) -> string:strip(String, left).
str_rtrim(String) -> string:strip(String, right).
%% remove other character
str_strip(String, Character) -> string:strip(String, both, Character).
str_lstrip(String, Character) -> string:strip(String, left, Character).
str_rstrip(String, Character) -> string:strip(String, right, Character).

regex_replace(String, Pattern, Replacement) ->
   re:replace(String, Pattern, Replacement, [{return, list}]).

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
string_to_number(L) when is_list(L) ->
   Float = (catch erlang:list_to_float(L)),
   case is_number(Float) of
      true -> Float;
      false -> Int = (catch erlang:list_to_integer(L)),
         case is_number(Int) of
            true -> Int;
            false -> false
         end
   end.

