%% Date: 04.01.17 - 16:01
%% Ⓒ 2017 LineMetrics GmbH
-module(dfs).
-author("Alexander Minichmair").

%% API
-export([test/0, test/1, lambda_test/0]).
-export([bool/1, int/1, float/1, string/1]).

lambda_test() ->
   L = "(2 *3 + math:sqrt(1.33*2.0005896742)/2) > 3 andalso S == \"server001.example.com\"",
   S = "fun(S) -> " ++ L ++ " end.",
   parse_fun(S).

parse_fun(S) ->
   {ok, Ts, _} = erl_scan:string(S),
   {ok, Exprs} = erl_parse:parse_exprs(Ts),
   {value, Fun, _} = erl_eval:exprs(Exprs, []),
   Fun.


test(string) ->
   String = "
   var inStreamId = '1.004.987349f9e87fwef'
   var outStreamId = '2.404.5dfgs555sa5df5a'
   var test = 5 > 3
   var in1 =
      |stream_in()
      .from(inStreamId)
      .deadman(15.0, 30.0)
      .as('LOW on CARBONIDE !!!')

   var in2 =
      |stream_in()
      .from('1.004.987349f9e87fwef')
      |deadman(15.0, 30s)

   in2
      |join(in1)
      .on('val')

      |lambda(lambda: in1.val+in2.val)

      |window()
      .every(15s)
      .period(30m)
      .stats(esp_mean, esp_difference)
      .field('val')
      .as('mean', 'diff')
      .align()

      |stream_out(outStreamId)
      .translate(5 > 'mean')

   in1
      @detectBoxOutage()
      .tolerance(17m)",

%%   String = "
%%   |window()
%%   .every(5+5)
%%   .period(30h) ",
%%   io:format("IN: ~p",[String]),
   {ok, Tokens, EndLine} = dfs_lexer:string(String),
%%   io:format("TOKENS: ~p~n" ,[Tokens])
%%   ,
   io:format("Parsed: ~n~p~n",[dfs_parser:parse(Tokens)]).
%%   parse(dfs_parser:parse(Tokens)).

test() ->
   {ok, Tokens, _EndLine} = parse(file, "src/test_script.dfs"),
   io:format("TOKENS: ~p~n" ,[Tokens])
   ,
   {ok, ParseTree} = dfs_parser:parse(Tokens),
   io:format("~n~nParsed: ~n~p~n",[ParseTree]),
   parse(ParseTree).



parse(file, FileName) ->
   {ok, Data} = file:read_file(FileName),
   io:format("FILEDATA: ~p",[Data]),
   StringData = binary_to_list(binary:replace(Data, <<"\\">>, <<"">>)),
   dfs_lexer:string(StringData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(Tree) when is_list(Tree) ->
   ets:new(dfs_parser, [set, public, named_table, {read_concurrency,true},{heir, self(), dfs_parser} ]),
   io:format("~n~nUSE:~n"),
   [parse(Stmt) || Stmt <- Tree];

parse({statement, {declarate, DecName, {chain, Chain}}}) ->
   ChainNodes = chain(Chain),
   save_chain_declaration(DecName, ChainNodes),
   {LastNodeName, _LNP, _NP} = lists:last(ChainNodes),
   io:format("stmt CHAIN DECLARATION: ~p [~p] ~n" ,[{DecName, ChainNodes}, LastNodeName]);
parse({statement, {declarate, DecName, DecValue}}) ->
   save_declaration(DecName, DecValue),
   io:format("stmt VALUE DECLARATION: ~p~n" ,[{DecName, DecValue}]);
parse({statement, {ident_expr, Identifier, {chain, Chain}}}) ->
%%   io:format("~n(param) identifier lookup for: ~p found: ~p~n",[Identifier, get_declaration(Identifier)]),
   ChainNodes = chain(Chain),
   L = lists:last(ChainNodes),
   {Node,_,_} = L,
   case get_declaration(Identifier) of
          nil -> erlang:error(io_lib:format("Undefined Identifier ~p used in chain expression",[Identifier]));
          {connect, Name} -> io:format("~n<identifier exp> connect node ~p to node ~p~n",[Name, Node])
         end,
   io:format("stmt IDENTIFIER EXPR: ~p ~n" ,[{Identifier, ChainNodes}])
.
%%;
%%   {Identifier, chain(Chain, [])}.
chain(ChainElements) when is_list(ChainElements) ->
   #{nodes := Nodes, current := CurrentNode} =
   lists:foldl(
      fun
         ({node, NodeName, {params, Params}}, #{nodes := [], current := {}}) ->
            #{nodes => [], current => {NodeName, params(Params), []}};
         ({node, NodeName, {params, Params}}, #{nodes := Ns, current := {_Node, _NodePars, _Pas}=NP}) ->
            io:format("~nconnect node ~p to node ~p~n",[NodeName, _Node]),
            #{nodes => Ns ++ [NP], current => {NodeName, params(Params), []}};
         ({node, NodeName}, #{nodes := [], current := {}}) ->
            #{nodes => [], current => {NodeName, [], []}};
         ({node, NodeName}, #{nodes := Ns, current := {_Node, _NodeParams, _Params}=CN}) ->
            io:format("~nconnect node ~p to node ~p~n",[NodeName, _Node]),
            #{nodes => Ns++[CN], current => {NodeName, [], []}};
         ({func, Name, {params, Params}}, #{current := {Node, NodeParams, Ps}}=Acc) ->
            Acc#{current := {Node, NodeParams, Ps++[{Name, params(Params)}]}};
         ({func, Name}, #{current := {Node, NodeParams, Ps}}=Acc) ->
            Acc#{current := {Node, NodeParams, Ps ++ [{Name, []}]}}
      end,
      #{nodes => [], current => {}},
      ChainElements
   ),
   AllNodes = Nodes ++ [CurrentNode],
   AllNodes.


params(Params) when is_list(Params)->
   [param(P) || P <- Params].

param({identifier, Ident}) ->
   io:format("~n(param) identifier lookup for: ~p found: ~p~n",[Ident, get_declaration(Ident)]),
   I = case get_declaration(Ident) of
          nil -> Ident;
          {connect, _} -> Ident;
%%          {string, _LN, Contents} -> "\"" ++ binary_to_list(Contents) ++ "\"";
%%          {string, Contents} -> "\"" ++ binary_to_list(Contents) ++ "\"";
          Other -> unwrap(Other)
      end,
   {identifier, I};
param({pfunc, {_N, {params, Ps}}}=L) ->
   param({lambda, [L]});
param({pfunc, N}) ->
   param({lambda, [{pfunc, {N,{params,[]}}}]});
param({lambda, LambdaList}) ->
%%   io:format("+++Lambda Elements: ~p~n",[LambdaList]),
{Lambda, BinRefs} =
      lists:foldl(
         fun(E, {L, Rs}) -> %io:format("LAMBDA so far ::: : ~p~n",[L]),
            Refs0 =
            case E of
               {reference, _LN, Ref}=_R ->
                  [Ref|Rs];
%%                 io:format("+++Lambda Reference: ~p~n",[R]);
               {pexp, Eles} -> %io:format("+++++++++++++++++++++++++++++++++++++++++++++Lambda PEXP: ~p~n",[Eles]),
                  NewPs = extract_refs(Eles),
                  NewPs++Rs;
               {pfunc, {_FName, {params, Params}}}=_P ->
%%                  io:format("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++Lambda fun Params: ~p~n",[_P]),
                  NewPs = extract_refs(Params),
                  NewPs++Rs;
               _ -> Rs
            end,
            {L++[lexp(E)], Refs0}
         end,{[], []},LambdaList), %% foldl
   %% unique params
   BRefs = sets:to_list(sets:from_list(BinRefs)),
   Refs = lists:map(fun(E) -> param_from_ref(E) end, BRefs),
%%   io:format("LAMBDA References ~p (~p)~n",[Refs, BinRefs]),
   {lambda, make_lambda_fun(lists:concat(Lambda), Refs), BRefs}
;
param({regex, Regex}) ->
%%   io:format("REGEX PARAM FOUND: ~p~n",[Regex]),
   {regex, Regex};
param(P) ->
%%   io:format("PARAM: ~p~n",[P]),
   P.


extract_refs(Elements) when is_list(Elements) ->
   lists:foldl(
      fun(E1, Acc) ->
         case E1 of
            {reference, _Ln, Ref1} -> [Ref1|Acc];
            {reference, Ref1} -> [Ref1|Acc];
            {pfunc, {_FName, {params, Params}}} -> lists:flatten([extract_refs(Params)|Acc]);
            {pexp, Eles} -> lists:flatten([extract_refs(Eles)|Acc]);
            {paren, Exp} -> lists:flatten([extract_refs(Exp)|Acc]);
            _O -> Acc
         end
      end,
      [],
      Elements
   );

extract_refs({reference, _Ln, Ref1}) ->
   Ref1;
extract_refs({reference, Ref1}) ->
   Ref1;
extract_refs({pexp, Elems}) when is_list(Elems) ->
   extract_refs(Elems);
extract_refs(_) -> [].

param_from_ref(Ref) when is_binary(Ref) ->
   Ref0 = binary:replace(Ref, <<".">>, <<"_">>),
   [Hd|RefString] = binary_to_list(Ref0),
   NewFirst = string:to_upper(Hd),
   [NewFirst|RefString].

make_lambda_fun(LambdaString, FunParams) ->
   Params = l_params(FunParams, []),
   F =  "fun(" ++ Params ++ ") -> " ++ LambdaString ++ " end.",
%%   Fun = parse_fun(F),
%%   io:format("Fun RETURN: ~p~n",[Fun(3)]),
   F
.

l_params([], Acc) ->
   Acc;
l_params([P], Acc) ->
   Acc++P;
l_params([P|Ps], Acc) ->
  l_params(Ps, Acc ++ P ++ ", ").

params_pfunc(Params) when is_list(Params) ->
%%   io:format("*************************************************************BEFORE prarms_pfunc~p~n",[Params]),
   P = lists:map(
      fun(E) -> param_pfunc(E) end,
      Params
   ),
   P1 = l_params(P, []),
%%   io:format("###########################################################AFTER prarms_pfunc~p~n",[P]),
lists:flatten(P1)
.
param_pfunc({identifier, Ident}) ->
%%   io:format("~n(param_func) identifier lookup for: ~p found: ~p~n",[Ident, get_declaration(Ident)]),
   case get_declaration(Ident) of
      nil -> binary_to_list(Ident);
      {connect, _} -> binary_to_list(Ident);
      {string, _LN, String} -> "\"" ++ binary_to_list(String) ++ "\"";
      Other -> binary_to_list(unwrap(Other))
   end;
param_pfunc({reference, Ref}) ->
%%   io:format("~n(param_func) found Reference: ~p~n",[Ref]),
   param_from_ref(Ref);
param_pfunc({string, _LN, Ref}) ->
   param_pfunc({string, Ref});
param_pfunc({string, Ref}) ->
%%   io:format("~n(param_func) string: ~p~n",[Ref]),
   "\"" ++ binary_to_list(Ref) ++ "\"";
param_pfunc({pexp, Elements}) ->
   [param_pfunc(E) || E <- Elements ];
param_pfunc(Other) ->
%%   io:format("[param_pfunc] ~p~n",[Other]),
      lexp(Other).


%% lambda primary expressions
lexp(Expressions) when is_list(Expressions) ->
%%   io:format("LAMBDA EXPRESSIONS: ~p~n",[Expressions]),
   lists:flatten([lexp(E) || E <- Expressions]);
%% parenthized expressions
lexp({paren, Exp}) ->
   "(" ++ lexp(Exp) ++ ")";
lexp({int, Int}) ->
   integer_to_list(Int);
lexp({int, _LN, Int}) ->
   integer_to_list(Int);
lexp({float, _LN, Float}) ->
   float_to_list(Float);
lexp({bool, _LN, Bool}) ->
   atom_to_list(Bool);
lexp({bool, Bool}) ->
   atom_to_list(Bool);
lexp({identifier, _LN, Id}) ->
%%   io:format("~n(lexp)identifier lookup for: ~p found: ~p~n",[Id, get_declaration(Id)]),
   case get_declaration(Id) of
      nil -> binary_to_list(Id);
      Other -> unwrap(Other)
   end;
lexp({reference, _LN, Ref}) ->
   param_from_ref(Ref);
lexp({operator, _LN, Op}) ->
   case Op of
      'AND' -> " andalso ";
      'OR'  -> " orelse ";
      '<='  -> " =< ";
      '=>'  -> " >= ";
      '!='  -> " /= ";
      '!'   -> " not ";
      _ -> " " ++ atom_to_list(Op) ++ " "
   end;
lexp({string, _LN, S}) ->
   "\"" ++ binary_to_list(S) ++ "\"";
lexp({string, S}) ->
   "\"" ++ binary_to_list(S) ++ "\"";
lexp({pexp, Elements}) when is_list(Elements) ->
   lists:concat([lexp(E) || E <- Elements]);
lexp({pexp, {pexp, Elements}}) when is_list(Elements) ->
   lists:concat([lexp(E) || E <- Elements]);
lexp({pfunc, {FName, {params, Params}}}) ->
%%   io:format("#+#+#+#+#+#+#+#++#pfunc FUNCTION PARAMS ~p~n",[Params]),
   Ps = params_pfunc(Params),
%%   io:format("pfunc PARAMS ~p~n",[Ps]),
   io:format("~n(lexp)check function is callable: ~p(~p) ~n",[binary_to_list(FName), Ps]),
   FuncName = pfunction(binary_to_list(FName), length(Params)),
   FuncName ++ "(" ++ Ps ++ ")";
lexp({pfunc, FName}) ->
   io:format("~n(lexp)check function is callable: ~p/0 ~n",[binary_to_list(FName)]),
   pfunction(binary_to_list(FName), 0) ++ "()".


save_declaration(Ident, Value) ->
   ets:insert(dfs_parser, {Ident, Value}).
save_chain_declaration(Ident, Nodes) when is_list(Nodes) ->
   LastNode = lists:last(Nodes),
   {NodeName, _Np, _NCP} = LastNode,
   ets:insert(dfs_parser, {Ident, {connect, NodeName}}).
get_declaration(Ident) ->
   case ets:lookup(dfs_parser, Ident) of
      [] -> nil;%%erlang:error("~nundefined declaration for ~p~n" ,[Ident]);
      [{Ident, {connect, {Name, _Connection}}}] -> {connect, Name};
      [{Ident, Value}] -> Value
   end.

unwrap({_T, _LN, Cotents}) ->
   Cotents;
unwrap({_T, Cotents}) ->
   Cotents;
unwrap(V) ->
   V.

%%%%%%%%%%%%%%%%%%%%%%%%%%% LAMBDA FUNCTIONS %%%%%%%%%%%%%%%%%%%
pfunction(FName, PCount) when is_list(FName) ->
   NN =
   case erlang:function_exported(?MODULE, list_to_atom(FName), PCount) of
      true -> FName;
      false -> Prefix =  string:sub_string(FName, 1, 4),
         case Prefix of
                  "str_" -> "string:" ++ string:sub_string(FName, 5, length(FName));
                  _W -> "math:" ++ FName
               end
   end,
   io:format("convert function name: ~p ==> ~p~n",[FName, NN]),
   NN.


%%%%%%%%%%%%%%%%%%%%%%%%%%%5 TYPE CONVERSIONS %%%
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
float(V) when is_float(V) ->
   V;
float(V) when is_integer(V) ->
   V0 = integer_to_list(V),
   V1 = V0 ++ ".0",
   list_to_float(V1);
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