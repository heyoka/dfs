%% v2
Nonterminals
dfscript statement statements declaration expression chain primary
primaryExpr function parameters parameter  .

Terminals
'=' '(' ')' ',' '[' ']' '{' '}' '.' '-' '!'
def node macro user_node string text identifier number bool
duration regex int stream_id float operator lambda inline reference.


Rootsymbol dfscript.

dfscript          -> statements : '$1'.
statements        -> statement : ['$1'].
statements        -> statement statements : ['$1'] ++ '$2'.

statement         -> declaration : {statement, '$1'}.
statement         -> expression : {statement, '$1'}.

declaration       -> 'def' identifier '=' expression : {declarate, unwrap('$2'), '$4'}.

expression        -> identifier chain : {ident_expr, unwrap('$1'), {chain, '$2'}}.
expression        -> function chain  : [unwrap1('$1')] ++ ['$2'].
expression        -> chain  : {chain, '$1'}.
expression        -> primary  : '$1'.
expression        -> primaryExpr : '$1'.
expression        -> '[' parameters ']' : {list, unwrap('$2')}.
expression        -> '{' parameters '}' : {tuple, unwrap('$2')}.
expression        -> lambda primaryExpr : {lambda, lists:flatten([unwrap('$2')])}.
expression        -> inline primaryExpr : {inline, lists:flatten([unwrap('$2')])}.

%chain             -> user_node function chain : [{unwrap('$1'), unwrap1('$2')}] ++ '$3'.

chain             -> user_node function chain : [unwrap_node_func({user_node, unwrap1('$2')})] ++ '$3'.
chain             -> macro function chain : [unwrap_node_func({macro, unwrap1('$2')})] ++ '$3'.
chain             -> node function chain : [unwrap_node_func({node, unwrap1('$2')})] ++ '$3'.
chain             -> '.' function chain : ['$2'] ++ '$3'.
chain             -> '.' identifier chain : ['$2'] ++ '$3'.
chain             -> '.' function : ['$2'].
chain             -> node function : [unwrap_node_func({node, unwrap1('$2')})].
chain             -> user_node function : [unwrap_node_func({user_node, unwrap1('$2')})].
chain             -> macro function : [unwrap_node_func({macro, unwrap1('$2')})].

primaryExpr      -> primaryExpr primaryExpr: ['$1']++['$2'].
primaryExpr      -> operator primary : {pexp, ['$1', '$2']}.
primaryExpr      -> operator function : ['$1'] ++ [{pfunc, unwrap1('$2')}].
primaryExpr      -> function primaryExpr: [{pfunc, unwrap1('$1')}, unwrap('$2')].
primaryExpr      -> operator primary primaryExpr: {pexp, ['$1', '$2', '$3']}.
primaryExpr      -> primary operator primary : {pexp, ['$1', '$2', '$3']}.
primaryExpr      -> primary operator primaryExpr : {pexp, ['$1', '$2', '$3']}.
%primaryExpr      -> primaryExpr operator primaryExpr : {pexp, ['$1', '$2', '$3']}.
primaryExpr      -> function : [{pfunc,unwrap1('$1')}].
%primaryExpr      -> identifier '(' primaryExpr ')' : {function, unwrap($1), {params, '$3'}}.
%primaryExpr      -> '(' primaryExpr ')' : {paren,'$2'}.
primaryExpr      -> '(' primary ')' : {paren,'$2'}.


function         -> identifier '(' parameters ')' : {func, unwrap('$1'), {params, unwrapParams('$3')}}.
function         -> identifier '(' ')' : {func, unwrap('$1')}.
primary          -> '(' primaryExpr ')' : {paren,'$2'}.
primary          -> duration : '$1'.
primary          -> '[' ']' : {list, []}.
primary          -> '{' '}' : {tuple, []}.
primary          -> number : '$1'.
primary          -> float : '$1'.
primary          -> int : '$1'.
primary          -> stream_id : '$1'.
primary          -> string : '$1'.
primary          -> text : '$1'.
primary          -> regex : '$1'.
primary          -> bool : '$1'.
primary          -> identifier : '$1'.
primary          -> identifier '.' identifier: {'$1', '$3'}.
primary          -> reference : '$1'.
primary          -> '-' primary : '$2'.
primary          -> '!' primary : '$2'.
primary          -> operator : '$1'.
parameters       -> parameter : ['$1'] .
parameters       -> parameter ',' parameters : ['$1'] ++ '$3'.
parameter        -> '[' parameters ']' : {list, unwrap('$2')}.
parameter        -> '{' parameters '}' : {tuple, unwrap('$2')}.
parameter        -> primaryExpr : '$1'.
parameter        -> primary : '$1'.
parameter        -> lambda primaryExpr : {lambda, lists:flatten([unwrap('$2')])}.
parameter        -> inline primaryExpr : {inline, lists:flatten([unwrap('$2')])}.


Erlang code.

unwrapParams(L) -> unwrapParams(L, []).
unwrapParams([], Acc) -> lists:flatten(lists:reverse(Acc));
unwrapParams([{lambda, _S}=P|R],Acc) -> unwrapParams(R, [P|Acc]);
unwrapParams([{inline, _S}=P|R],Acc) -> unwrapParams(R, [P|Acc]);
unwrapParams( [{N,_,V}|R], Acc) -> unwrapParams(R, [{N,V}|Acc]);
unwrapParams([{primary_exp, _F, _S, _L}=P|R], Acc) -> unwrapParams(R, [P|Acc]);
unwrapParams([_Exp=P|R], Acc) -> unwrapParams(R, [P|Acc]).
unwrap1({_,Name}) -> Name;
unwrap1({_,Name,{params,Params}}) -> {Name, {params, Params}}.
unwrap({primary_exp,V}) -> V;
unwrap({pexp,_V}=P) -> P;
unwrap({_,_,V}) -> V;
unwrap(V) when is_list(V) -> lists:flatten(V);
unwrap(V) when is_tuple(V) -> V.
unwrap_node_func({node, {N, P}}) -> {node, N, P};
unwrap_node_func({user_node, {N, P}}) -> {user_node, N, P};
unwrap_node_func({macro, {N, P}}) -> {macro, N, P};
unwrap_node_func(V) -> V.