%% Date: 04.01.17 - 16:01
%% Ⓒ 2017 heyoka
-module(dfs).
-author("Alexander Minichmair").

%% API
-export([parse/1, parse/2, parse/3, parse_file/1, parse_file/2, parse_file/3]).

-export([test/0
   , string_test/0
   , user_node/1, test/1]).

%% testing value overriding, here 'threshold' from the example script will be
%% replaced with a value of 111
%% the replacement value must match exactly the original values datatpye (int, float, string, ...)
test(FileName) ->
   parse_file(FileName, [], [{<<"threshold">>, 111}, {<<"mylist">>,[5,6,7,8]}, {<<"func">>, <<"lambda: \"rate\" * 9">>}]).
test() ->
   test("src/test_script.dfs")
%%   ,
%%   string_test().
.

string_test() ->
   parse(

     "
      def m = 'pm'
      def inStreamId = '1.004.987349f9e87fwef'
      def outStreamId = '2.404.5dfgs555sa5df5a'
      % false lambda: must be speicifed before primary-expr
      def host = 'de.example.com'
      def dead_message = 'LOW on CARBONIDE !!!'
      def deadman_period = 15m
      def bool = TRUE
      def number = 33.4434
      def in1 =
         |stream_in(bool)
         .from(inStreamId)

      def in2 =
         |stream_in()
         .from('1.004.987349f9e87fwef')

      in2
         |join(in1)
         .on('val')
         .translate(
            ?_chair?, %% 'red_chair', red_desk
            lambda: \"opmean.val\" + 2 * max(\"val1\", \"val2\")/3,
            myNextParam,
            lambda: (\"max\" - \"min\") /2,
            lambda: string(\"max\") == '5',
            lambda: str_ends_with(inStreamId, 'fwef'),
            lambda: str_ends_with(\"muxolo\", 'olo'),
            lambda: lookup(deadman_period) + number,
            lambda: string(bool)
           )

         |lambda(
            lambda: str_trim(\"host\") == 'server001.example.com',
            mode,
            '12354688978' %% numberstring here
         )"
   ).

-spec parse(list()) -> list().
parse_file(FileName) when is_list(FileName) ->
   parse_file(FileName, [], []).
parse_file(FileName, Libs) ->
   parse_file(FileName, Libs, []).
parse_file(FileName, Libs, Replacements) ->
   {ok, Data} = file:read_file(FileName),
   StringData = binary_to_list(binary:replace(Data, <<"\\">>, <<>>, [global])),
   parse(StringData, Libs, Replacements).

parse(StringData) ->
   parse(StringData, [], []).

parse(D, Libs) ->
   parse(D, Libs, []).

-spec parse(binary()|list(), list(), list()) -> list().
parse(Binary, Libs, Replacements) when is_binary(Binary) ->
   parse(binary_to_list(Binary), Libs, Replacements);
parse(String, Libs, Replacements) when is_list(String) andalso is_list(Libs) ->
   LambdaLibs = [dfs_std_lib, estr] ++ [Libs],
   FLibs = lists:flatten(LambdaLibs),
   %% ensure libs are there for us
   lists:foreach(fun(E) -> code:ensure_loaded(E) end, FLibs),
   ets:new(?MODULE, [set, public, named_table]),
   ets:insert(?MODULE, {lfunc, FLibs}),
   Rep = [{RName, prepare_replacement(RName, Repl)} || {RName, Repl} <- Replacements],
   ets:insert(?MODULE, {replace_def, Rep} ),
   Res =
   case dfs_lexer:string(String) of
      {ok, Tokens, _EndLine} ->
         case dfs_parser:parse(Tokens) of
            {ok, Data} ->
%%               try eval(Data) of
%%                  Result -> Result
%%               catch
%%                  throw:Error -> {error, Error}
%%               end;
               eval(Data);
            {error, {LN, dfs_parser, Message}} ->
               {{parser_error, line, LN}, Message};
            Error -> Error
         end;
      {error, {LN, dfs_lexer, Message}, _LN} -> {{lexer_error, line, LN}, Message};
      Err -> Err
   end,
   ets:delete(?MODULE),
   Res.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prepare_replacement(Name, Repl) when is_binary(Repl) ->
   prepare_replacement(Name, binary_to_list(Repl));
prepare_replacement(Name, Repl) when is_list(Repl) ->
   parse_replacement(Name, Repl);
prepare_replacement(_Name, Repl) ->
   Repl.
parse_replacement(_Name, ("lambda:" ++ _R) = String ) ->
   case dfs_lexer:string(String) of
      {ok, Tokens, _EndLine} ->
         case dfs_parser:parse(Tokens) of
            {ok, [{statement, Data}]}->
               param(Data);
            {error, {LN, dfs_parser, Message}} ->
               {{parser_error, line, LN}, Message};
            Error -> Error
         end;
      {error, {LN, dfs_lexer, Message}, _LN} -> {{lexer_error, line, LN}, Message};
      Err -> Err
   end;

parse_replacement(Name, L) when is_list(L) ->
   check_list_types(Name, L);
parse_replacement(_Name, R) -> R.

check_list_types(Name, L) ->
   case list_type(L) of
      true -> L;
      false -> throw([<<"list_contains_mixed_types">>, Name, L])
   end.
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
   Data;

%% @doc chain declaration
eval({statement, {declarate, DecName, {chain, Chain}}}) ->
   {{nodes, ChainNodes}, {connections, _Connections}} = C = chain(Chain),
   save_chain_declaration(DecName, ChainNodes),
   C;
%% @doc chain statement without declaration
eval({statement, {chain, Chain}}) ->
   {{nodes, _ChainNodes}, {connections, _Connections}} = C = chain(Chain),
   C;
%% @doc chain declaration connected to previously declared identifier
eval({statement, {declarate, DecName, {ident_expr, Identifier, {chain, Chain}}}}) ->
   {{nodes, ChainNodes}, {connections, Connections}} = _Cs = chain(Chain),
   save_chain_declaration(DecName, ChainNodes),
   {Node,_,_} = hd(ChainNodes),
   NewConns =
      case get_declaration(Identifier) of
         nil -> throw("Undefined Identifier \"" ++ binary_to_list(Identifier) ++ "\" used in chain expression");
         {connect, Name} ->
            [{Node,Name}|Connections]
      end,
   {{nodes, ChainNodes}, {connections, NewConns}};
eval({statement, {declarate, DecName, {list, DecValues}}}) ->
   check_list_types(DecName, DecValues),
   NewValues = params(DecValues),
   save_declaration(DecName, NewValues);
eval({statement, {declarate, DecName, {lambda, _DecValue}=L}}) ->
   save_declaration(DecName, param(L));
eval({statement, {declarate, DecName, DecValue}}) ->
   save_declaration(DecName, DecValue);
eval({statement, {ident_expr, Identifier, {chain, Chain}}}) ->
   {{nodes, ChainNodes}, {connections, Connections}} = chain(Chain),
   {Node,_,_} = hd(ChainNodes),
   NewConns =
   case get_declaration(Identifier) of
          nil -> throw("Undefined Identifier \"" ++ binary_to_list(Identifier) ++ "\" used in chain expression");
          {connect, Name} -> [{Node,Name}|Connections]
         end,
   {{nodes, ChainNodes}, {connections, NewConns}};
eval({statement, What}) ->
   erlang:error(iolist_to_binary([<<"Syntax error, illegal statement in script: ">>,io_lib:format("~p",[What])])).
%%;
%%   {Identifier, chain(Chain, [])}.
chain(ChainElements) when is_list(ChainElements) ->
   #{nodes := Nodes, current := CurrentNode, conns := Connections} =
   lists:foldl(
      fun
         ({node, NodeName, {params, Params}}, #{nodes := [], current := {}}=Acc) ->
            Id = node_id(),
            Acc#{nodes => [], current => {{NodeName, Id}, params(Params), []}};
         ({node, NodeName, {params, Params}}, #{nodes := Ns, current := {Node, _NodePars, _Pas}=NP,
            conns := Cs}=Acc) ->
            %io:format("~nconnect node ~p to node ~p~n",[NodeName, _Node]),
            Id = node_id(),
            Acc#{nodes => (Ns ++ [NP]), current => {{NodeName, Id}, params(Params), []},
               conns => [{{NodeName, Id}, Node}|Cs]};
         ({node, NodeName}, #{nodes := [], current := {}}=Acc) ->
            Id = node_id(),
            Acc#{nodes => [], current => {{NodeName,Id}, [], []}};
         ({node, NodeName}, #{nodes := Ns, current := {Node, _NodeParams, _Params}=CN,
            conns := Cs}=Acc) ->
            Id = node_id(),
%%            io:format("~nconnect node ~p to node ~p~n",[NodeName, _Node]),
            Acc#{nodes => Ns++[CN], current => {{NodeName, Id}, [], []},
               conns => [{{NodeName,Id}, Node}|Cs]};
         ({user_node, NodeName, {params, Params}}, #{nodes := [], current := {}}=Acc) ->
            Id = node_id(),
            Acc#{nodes => [], current => {{user_node(NodeName), Id}, params(Params), []}};
         ({user_node, NodeName, {params, Params}}, #{nodes := Ns, current := {Node, _NodePars, _Pas}=NP,
            conns := Cs}=Acc) ->
            %io:format("~nconnect node ~p to node ~p~n",[NodeName, _Node]),
            Id = node_id(),
            %io:format("user_node_name: ~p~n",[NodeName]),
            Acc#{nodes => (Ns ++ [NP]), current => {{user_node(NodeName), Id},
               params(Params), []}, conns => [{{user_node(NodeName), Id}, Node}|Cs]};
         ({user_node, NodeName}, #{nodes := [], current := {}}=Acc) ->
            Id = node_id(),
            Acc#{nodes => [], current => {{user_node(NodeName),Id}, [], []}};
         ({user_node, NodeName}, #{nodes := Ns, current := {Node, _NodeParams, _Params}=CN,
            conns := Cs}=Acc) ->
            Id = node_id(),
%%            io:format("~nconnect node ~p to node ~p~n",[NodeName, _Node]),
            Acc#{nodes => Ns++[CN], current => {{user_node(NodeName), Id}, [], []},
               conns => [{{user_node(NodeName),Id}, Node}|Cs]};
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
   lists:flatten([param(P) || P <- Params]).

param({identifier, Ident}) ->
%%   io:format("~n(param) identifier lookup for: ~p found: ~p~n",[Ident, get_declaration(Ident)]),
   case get_declaration(Ident) of
      nil -> {identifier, Ident};
      {connect, _} = C -> C;
      {Type, _LN, Val} -> {Type, Val};
      {Type, Val} -> {Type, Val};
      List when is_list(List) -> [{Type, Val} || {Type, _LN, Val} <- List];
      {lambda, _, _, _} = Lambda -> Lambda
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

%% save a simple declaration,
%% here is where declaration - overwriting happens,
%% you know for templates: every declaration (def keyword) which is not a chain-declaration
%% can be overwritten with a custom value

save_declaration(Ident, [{VType, VLine, _Val}|_R]=Vals) when is_list(Vals) ->
   [{replace_def, Replacements}] = ets:lookup(?MODULE, replace_def),
   RVal = proplists:get_value(Ident, Replacements, norepl),
   %io:format("Replacements ~p~nKey: ~p~nrval: ~p~n~p",[Replacements, Ident, RVal, Vals]),
   NewValue =
      case RVal of
         norepl -> Vals;
         NVal  -> [{VType, VLine, V} || V <- NVal]
      end,
   ets:insert(?MODULE, {Ident, NewValue});
save_declaration(Ident, {lambda, _Fun, _Decs, _Refs}=Value) ->
   [{replace_def, Replacements}] = ets:lookup(?MODULE, replace_def),
   RVal = proplists:get_value(Ident, Replacements, norepl),
   %io:format("Replacements ~p~nKey: ~p~nrval: ~p~n~p",[Replacements, Ident, RVal, Value]),
   NewValue =
      case RVal of
         norepl -> Value;
         NVal  -> {lamdba, NVal}
      end,
   ets:insert(?MODULE, {Ident, NewValue});
save_declaration(Ident, {VType, VLine, _Val}=Value) ->
   [{replace_def, Replacements}] = ets:lookup(?MODULE, replace_def),
   RVal = proplists:get_value(Ident, Replacements, norepl),
   %io:format("Replacements ~p~nKey: ~p~nrval: ~p~n~p",[Replacements, Ident, RVal, Value]),
   NewValue =
   case RVal of
      norepl -> Value;
      NVal  -> {VType, VLine, NVal}
   end,
   ets:insert(?MODULE, {Ident, NewValue}).
save_chain_declaration(Ident, Nodes) when is_list(Nodes) ->
   LastNode = lists:last(Nodes),
   {NodeName, _Np, _NCP} = LastNode,
   ets:insert(?MODULE, {Ident, {connect, NodeName}}).
get_declaration(Ident) ->
%%   io:format("~nget_declaration: ~p~n",[ets:lookup(dfs_parser, Ident)]),
   case ets:lookup(?MODULE, Ident) of
      [] -> nil;
      [{Ident, {connect, {_Name, _Connection}=N}}] -> {connect, N};
      [{Ident, Value}] -> %io:format("get_declaration value: ~p~n",[Value]),
         Value
   end.

unwrap({_T, _LN, Cotents}) ->
   Cotents;
unwrap({_T, Cotents}) ->
   Cotents;
unwrap(V) ->
   V.

user_node(Name) ->
   << <<"@">>/binary, Name/binary>>.

list_type([{_Type, _LN, _Val}|_R] = L) ->
   {_, _, Values} = lists:unzip3(L),
   list_type(Values);
list_type([{_Type, _Val}|_R] = L) ->
   {_, Values} = lists:unzip(L),
   list_type(Values);
list_type([E|R]) when is_number(E) ->
   lists:all(fun(El) -> is_number(El) end ,R);
list_type([E|R]) when is_list(E) ->
   lists:all(fun(El) -> is_list(El) end ,R);
list_type([E|R]) when is_binary(E) ->
   lists:all(fun(El) -> is_binary(El) end ,R);
list_type([E|R]) when is_atom(E) ->
   lists:all(fun(El) -> is_atom(El) end ,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%% LAMBDA FUNCTIONS %%%%%%%%%%%%%%%%%%%
pfunction(FName, Arity) when is_list(FName) ->
   NameAtom = list_to_atom(FName),
   [{lfunc, Modules}] = ets:lookup(?MODULE, lfunc),
%%   io:format("models are ~p~n",[Modules]),
   NN0 = lists:foldl(
     fun
        (_E, {done, _Module}=M) -> M;
        (E, Module) ->
           case erlang:function_exported(E, NameAtom, Arity) of
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
      nil -> case erlang:function_exported(math, NameAtom, Arity) of
                true -> "math:" ++ FName;
                false -> FName %throw("Function " ++ FName ++ " not found in library")
             end;
      {done, Else} -> Else
   end,
%%   io:format("convert function name: ~p ==> ~p~n",[FName, NN]),
   NN.