%% Date: 04.01.17 - 16:01
%% Ⓒ 2017 heyoka
-module(dfs).
-author("Alexander Minichmair").

%% API
-export([parse/1, parse/2]).

-export([test/0]).

test() ->
   parse("src/test_script.dfs").

-spec parse(list()) -> list().
parse(FileName) when is_list(FileName) ->
   parse(FileName, []).

-spec parse(list(), list()) -> list().
parse(FileName, Libs) when is_list(FileName) andalso is_list(Libs) ->
   LambdaLibs = [dfs_std_lib, estr] ++ [Libs],
   FLibs = lists:flatten(LambdaLibs),
   %% ensure libs are there for us
   lists:foreach(fun(E) -> code:ensure_loaded(E) end, FLibs),
   ets:new(?MODULE, [set, public, named_table]),
   ets:insert(?MODULE, {lfunc, FLibs}),

   Res =
   case parse_file(FileName) of
      {ok, Tokens, _EndLine} ->
         %io:format("~n~nTOKENS: ~p~n~n",[Tokens]),
         case dfs_parser:parse(Tokens) of
            {ok, Data} -> %io:format("~nDATA: ~n~p~n",[Data]),
               eval(Data);
%%               case (catch parse(Data)) of
%%                             Statements when is_list(Statements) -> Statements;
%%                             {'EXIT', {Message, _Trace}} -> {error, Message};
%%                             Error1 -> Error1
%%                          end;

            {error, {LN, dfs_parser, Message}} -> {{parser_error, line, LN}, Message};
            Error -> Error
         end;
      {error, {LN, dfs_lexer, Message}, _LN} -> {{lexer_error, line, LN}, Message};
      Err -> Err
   end,

   ets:delete(?MODULE),
   Res.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_file(FileName) ->
   {ok, Data} = file:read_file(FileName),
   StringData = binary_to_list(binary:replace(Data, <<"\\">>, <<>>)),
   dfs_lexer:string(StringData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval(Tree) when is_list(Tree) ->
   Data = lists:foldl(
      fun(E, {Ns, Cs}=A) ->
         case eval(E) of
            {{nodes, Nodes}, {connections, Connections}} -> {Ns++Nodes, Cs++Connections};
            _ -> A
         end
      end,
      {[],[]},
      Tree
   ),

   io:format("~nNODE + Conn DATA:~n~p", [Data]),
   Data;

eval({statement, {declarate, DecName, {chain, Chain}}}) ->
   {{nodes, ChainNodes}, {connections, _Connections}} = C = chain(Chain),
   save_chain_declaration(DecName, ChainNodes),
%%   {LastNodeName, _LNP, _NP} = lists:last(ChainNodes),
%%   io:format("stmt CHAIN DECLARATION: ~p  ~n" ,[{DecName, ChainNodes}]),
   C;
eval({statement, {declarate, DecName, DecValue}}) ->
   save_declaration(DecName, DecValue);
eval({statement, {ident_expr, Identifier, {chain, Chain}}}) ->
%%   io:format("~n(param) identifier lookup for: ~p found: ~p~n",[Identifier, get_declaration(Identifier)]),
   {{nodes, ChainNodes}, {connections, Connections}} = chain(Chain),
   {Node,_,_} = hd(ChainNodes),
   NewConns =
   case get_declaration(Identifier) of
          nil -> erlang:error("Undefined Identifier \"" ++ binary_to_list(Identifier) ++ "\" used in chain expression");
          {connect, Name} -> %io:format("~n<identifier exp> connect node ~p to node ~p~n",[Name, Node]),
                              [{Node,Name}|Connections]
         end,
%%   io:format("stmt IDENTIFIER EXPR: ~p ~n" ,[{Identifier, {{nodes, ChainNodes}, {connections, NewConns}}}]),
   {{nodes, ChainNodes}, {connections, NewConns}}
.
%%;
%%   {Identifier, chain(Chain, [])}.
chain(ChainElements) when is_list(ChainElements) ->
   #{nodes := Nodes, current := CurrentNode, conns := Connections} =
   lists:foldl(
      fun
         ({node, NodeName, {params, Params}}, #{nodes := [], current := {}}=Acc) ->
            Id = node_id(),
            Acc#{nodes => [], current => {{NodeName, Id}, params(Params), []}};
         ({node, NodeName, {params, Params}}, #{nodes := Ns, current := {Node, _NodePars, _Pas}=NP,conns := Cs}=Acc) ->
            %io:format("~nconnect node ~p to node ~p~n",[NodeName, _Node]),
            Id = node_id(),
            Acc#{nodes => (Ns ++ [NP]), current => {{NodeName, Id}, params(Params), []}, conns => [{{NodeName, Id}, Node}|Cs]};
         ({node, NodeName}, #{nodes := [], current := {}}=Acc) ->
            Id = node_id(),
            Acc#{nodes => [], current => {{NodeName,Id}, [], []}};
         ({node, NodeName}, #{nodes := Ns, current := {Node, _NodeParams, _Params}=CN, conns := Cs}=Acc) ->
            Id = node_id(),
%%            io:format("~nconnect node ~p to node ~p~n",[NodeName, _Node]),
            Acc#{nodes => Ns++[CN], current => {{NodeName, Id}, [], []}, conns => [{{NodeName,Id}, Node}|Cs]};
         ({func, Name, {params, Params}}, #{current := {Node, NodeParams, Ps}}=Acc) ->
            Acc#{current := {Node, NodeParams, Ps++[{Name, params(Params)}]}};
         ({func, Name}, #{current := {Node, NodeParams, Ps}}=Acc) ->
            Acc#{current := {Node, NodeParams, Ps ++ [{Name, []}]}}
      end,
      #{nodes => [], current => {}, conns => []},
      ChainElements
   ),
   AllNodes = Nodes ++ [CurrentNode],
%%   io:format(" Chain: ~p",[{{nodes, AllNodes}, {connections, Connections}}]),
   {{nodes, AllNodes}, {connections, Connections}}.

node_id() ->
   erlang:unique_integer([positive,monotonic]).

params(Params) when is_list(Params)->
   [param(P) || P <- Params].

param({identifier, Ident}) ->
%%   io:format("~n(param) identifier lookup for: ~p found: ~p~n",[Ident, get_declaration(Ident)]),
   case get_declaration(Ident) of
          nil -> {identifier,Ident};
          {connect, _} = C -> C;
          {Type, _LN, Val} -> {Type, Val}
   end;
param({pfunc, {_N, {params, _Ps}}}=L) ->
   param({lambda, [L]});
param({pfunc, N}) ->
   param({lambda, [{pfunc, {N,{params,[]}}}]});
param({lambda, LambdaList}) ->
%%   io:format("param: lambda ~p~n",[LambdaList]),
   {Lambda, BinRefs} =
      lists:foldl(
         fun(E, {L, Rs}) ->
            Refs0 =
            case E of
               {reference, _LN, Ref}=_R ->
                  [Ref|Rs];
               {pexp, Eles} ->
                  NewPs = extract_refs(Eles),
                  NewPs++Rs;
               {pfunc, {_FName, {params, Params}}}=_P ->
                  NewPs = extract_refs(Params),
                  NewPs++Rs;
               _ -> Rs
            end,
%%            io:format("param lexp(~p ++ ~p~n): n",[lexp(E)]),
            {L++[lexp(E)], Refs0}
         end,{[], []},LambdaList), %% foldl
   %% unique params
   BRefs = sets:to_list(sets:from_list(BinRefs)),
   Refs = lists:map(fun(E) -> param_from_ref(E) end, BRefs),
%%   io:format("LAMBDA ~p (~p)~n",[lists:concat(Lambda), BRefs]),
   {lambda, lists:concat(Lambda), BRefs, Refs}
;
param({regex, Regex}) ->
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

l_params([], Acc) ->
   Acc;
l_params([P], Acc) ->
   Acc++P;
l_params([P|Ps], Acc) ->
  l_params(Ps, Acc ++ P ++ ", ").

params_pfunc(Params) when is_list(Params) ->
   P = lists:map(
      fun(E) -> param_pfunc(E) end,
      Params
   ),
   P1 = l_params(P, []),
   lists:flatten(P1)
.
param_pfunc({identifier, _LN, Ident}) ->
   %io:format("identifier lookup for: ~p", [Ident]),
   param_pfunc({identifier, Ident});
%%param_pfunc({identifier, {identifier, 0, Ident}}) ->
%%   param_pfunc({identifier, Ident});
param_pfunc({identifier, Ident}) ->
   %io:format("~n(param_func) identifier lookup for: ~p found: ~p~n",[Ident, get_declaration(Ident)]),
   case get_declaration(Ident) of
      nil -> binary_to_list(Ident);
      {connect, _} -> binary_to_list(Ident);
      {string, _LN, String} -> "<<\"" ++ binary_to_list(String) ++ "\">>";
      {string, String} -> "<<\"" ++ binary_to_list(String) ++ "\">>";
      {duration, _LN, Dur} -> "<<\"" ++ binary_to_list(Dur) ++ "\">>";
      {bool, _LN, Bool} -> atom_to_list(Bool);
      {int, _LN, Int} -> integer_to_list(Int);
      {float, _LN, F} -> float_to_list(F);
      Other -> binary_to_list(unwrap(Other))
   end;
param_pfunc({reference, Ref}) ->
%%   io:format("~n(param_func) found Reference: ~p~n",[Ref]),
   param_from_ref(Ref);
param_pfunc({string, _LN, Ref}) ->
   param_pfunc({string, Ref});
param_pfunc({string, Ref}) ->
   "<<\"" ++ binary_to_list(Ref) ++ "\">>";
param_pfunc({pexp, Elements}) ->
   [param_pfunc(E) || E <- Elements ];
param_pfunc(Other) ->
   %io:format("[param_pfunc] ~p~n",[Other]),
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
   lexp({float, Float});
lexp({float, Float}) ->
   float_to_list(Float);
lexp({bool, _LN, Bool}) ->
   atom_to_list(Bool);
lexp({bool, Bool}) ->
   atom_to_list(Bool);
lexp({identifier, _LN, Id}) ->
   %io:format("[lexp({identifier] ~p~n",[Id]),
   param_pfunc({identifier, Id});
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
lexp({duration, _LN, S}) ->
   lexp({duration, S});
lexp({duration, S}) ->
   lexp({string, S});
lexp({string, _LN, S}) ->
   lexp({string, S});
lexp({string, S}) ->
   %io:format("~nlexp string ~p~n",[S]),
   "<<\"" ++ binary_to_list(S) ++ "\">>";
lexp({pexp, Elements}) when is_list(Elements) ->
   lists:concat([lexp(E) || E <- Elements]);
lexp({pexp, {pexp, Elements}}) when is_list(Elements) ->
   lists:concat([lexp(E) || E <- Elements]);
lexp({pfunc, {<<"if">>, {params, Params}}}) ->
   P = lists:map(
      fun(E) -> P0 = param_pfunc(E), %io:format("param_pfunc: ~p, ~p ~n",[E, P0]),
                P0 end,
      Params
   ),
   [Expr, PTrue, PFalse] = P,% f = l_params(P, []),
   F0 = "case " ++ lists:flatten(Expr) ++ " of true -> " ++ PTrue ++ "; false -> " ++ PFalse ++ " end",
%%   io:format("Lambda IF fun : ~p~n",[lists:flatten(F0)]),
   F0;
lexp({pfunc, {FName, {params, Params}}}) ->
   %io:format("Lambda fun name is : ~p ~n",[FName]),
   Ps = params_pfunc(Params),
   FuncName = pfunction(binary_to_list(FName), length(Params)),
   FuncName ++ "(" ++ Ps ++ ")";
lexp({pfunc, FName}) ->
   pfunction(binary_to_list(FName), 0) ++ "()".


save_declaration(Ident, Value) ->
   ets:insert(?MODULE, {Ident, Value}).
save_chain_declaration(Ident, Nodes) when is_list(Nodes) ->
   LastNode = lists:last(Nodes),
   {NodeName, _Np, _NCP} = LastNode,
   ets:insert(?MODULE, {Ident, {connect, NodeName}}).
get_declaration(Ident) ->
%%   io:format("~nget_declaration: ~p~n",[ets:lookup(dfs_parser, Ident)]),
   case ets:lookup(?MODULE, Ident) of
      [] -> nil;
      [{Ident, {connect, {_Name, _Connection}=N}}] -> {connect, N};
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
   NameAtom = list_to_atom(FName),
   [{lfunc, Modules}] = ets:lookup(?MODULE, lfunc),
%%   io:format("models are ~p~n",[Modules]),
   NN0 = lists:foldl(
     fun
        (_E, {done, _Module}=M) -> M;
        (E, Module) ->
           case erlang:function_exported(E, NameAtom, PCount) of
              true -> F0 = {done, atom_to_list(E) ++ ":" ++ FName},
                 %io:format("~p :: ~p ~n",[FName, F0]),
              F0;
              false -> Module
           end
     end,
      nil,
      Modules
   ),
   NN =
   case NN0 of
      nil -> case erlang:function_exported(math, NameAtom, PCount) of
                true -> "math:" ++ FName;
                false -> FName %erlang:error("Function " ++ FName ++ " not found in library")
             end;
      {done, Else} -> Else
   end,
%%   io:format("convert function name: ~p ==> ~p~n",[FName, NN]),
   NN.