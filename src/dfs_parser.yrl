%% v2
Nonterminals
dfscript statement statements declaration expression chain string_list
string_list_items string_list_item primary
primaryExpr function parameters parameter  .

Terminals
'=' '(' ')' ',' '[' ']' '.' '-' '!'
var def node user_node string text identifier number bool
duration regex int stream_id float operator lambda reference if.


Rootsymbol dfscript.

dfscript          -> statements : '$1'.
statements        -> statement : ['$1'].
statements        -> statement statements : ['$1'] ++ '$2'.

statement         -> declaration : {statement, '$1'}.
statement         -> expression : {statement, '$1'}.

declaration       -> 'var' identifier '=' expression : {declarate, unwrap('$2'), '$4'}.
declaration       -> 'def' identifier '=' expression : {declarate, unwrap('$2'), '$4'}.

expression        -> identifier chain : {ident_expr, unwrap('$1'), {chain, '$2'}}.
expression        -> function chain  : [unwrap1('$1')] ++ ['$2'].
expression        -> chain  : {chain, '$1'}.
expression        -> primary  : '$1'.
expression        -> primaryExpr : '$1'.
expression        -> string_list : '$1'.

chain             -> user_node function chain : [{unwrap('$1'), unwrap1('$2')}] ++ '$3'.
chain             -> node function chain : [unwrap_node_func({node, unwrap1('$2')})] ++ '$3'.
chain             -> '.' function chain : ['$2'] ++ '$3'.
chain             -> '.' identifier chain : ['$2'] ++ '$3'.
chain             -> '.' function : ['$2'].
chain             -> node function : [unwrap_node_func({node, unwrap1('$2')})].

%primaryExpr      -> 'if' '(' primaryExpr ',' primary ',' primary ')' : [{'if', unwrap('$3'), unwrap('$5'), unwrap('$7')}].
primaryExpr      -> primaryExpr primaryExpr: ['$1']++['$2'].
primaryExpr      -> operator primary : {pexp, ['$1', '$2']}.
primaryExpr      -> operator function : ['$1'] ++ [{pfunc, unwrap1('$2')}].
primaryExpr      -> function primaryExpr: [{pfunc, unwrap1('$1')}, unwrap('$2')].
%primaryExpr      -> operator primary operator: {pexp, ['$1', '$2', '$3']}.
primaryExpr      -> operator primary primaryExpr: {pexp, ['$1', '$2', '$3']}.
primaryExpr      -> primary operator primary : {pexp, ['$1', '$2', '$3']}.
primaryExpr      -> function : [{pfunc,unwrap1('$1')}].
%primaryExpr      -> identifier '(' primaryExpr ')' : {function, unwrap($1), {params, '$3'}}.

function         -> identifier '(' parameters ')' : {func, unwrap('$1'), {params, unwrapParams('$3')}}.
function         -> identifier '(' ')' : {func, unwrap('$1')}.
primary          -> '(' primaryExpr ')' : {paren,'$2'}.
primary          -> '(' primary ')' : {paren, '$2'}.
primary          -> duration : '$1'.
primary          -> number : '$1'.
primary          -> float : '$1'.
primary          -> int : '$1'.
primary          -> stream_id : '$1'.
primary          -> string : '$1'.
primary          -> text : '$1'.
primary          -> regex : '$1'.
primary          -> bool : '$1'.
%primary          -> primary_function : '$1'.
primary          -> identifier : '$1'.
primary          -> identifier '.' identifier: {'$1', '$3'}.
primary          -> reference : '$1'.
primary          -> '-' primary : '$2'.
primary          -> '!' primary : '$2'.
primary          -> operator : '$1'.
parameters       -> parameter : ['$1'] .
parameters       -> parameter ',' parameters : ['$1'] ++ '$3'.
parameter        -> primaryExpr : '$1'.
parameter        -> primary : '$1'.
parameter        -> lambda 'if' '(' primaryExpr ',' primary ',' primary ')' : [{'if', unwrap('$4'), unwrap('$6'), unwrap('$8')}].
parameter        -> lambda primaryExpr : {lambda, lists:flatten([unwrap('$2')])}.
string_list       -> '[' string_list_items ']' : '$2'.
string_list_items -> string_list_item : ['$1'] .
string_list_items -> string_list_item ',' string_list_items : ['$1'] ++ '$3' .
string_list_item -> string : unwrap('$1').
string_list_item -> identifier : unwrap('$1').


Erlang code.

unwrapParams(L) -> unwrapParams(L, []).
unwrapParams([], Acc) -> lists:flatten(lists:reverse(Acc));
unwrapParams([{lambda, _S}=P|R],Acc) -> unwrapParams(R, [P|Acc]);
unwrapParams( [{N,_,V}|R], Acc) -> unwrapParams(R, [{N,V}|Acc]);
unwrapParams([{primary_exp, _F, _S, _L}=P|R], Acc) -> unwrapParams(R, [P|Acc]);
unwrapParams([_Exp=P|R], Acc) -> unwrapParams(R, [P|Acc]);
unwrapParams([{lamdba, S}=P|R], Acc) -> {lambda, S};
unwrapParams([V|R], Acc) when is_list(V) -> unwrapParams(R, [lists:flatten(V)|Acc]).
unwrap1({_,Name}) -> Name;
unwrap1({_,Name,{params,Params}}) -> {Name, {params, Params}}.
unwrap({primary_exp,V}) -> V;
unwrap({pexp,_V}=P) -> P;
unwrap({_,_,V}) -> V;
unwrap(V) when is_list(V) -> lists:flatten(V).
unwrap_node_func({node, {N, P}}) -> {node, N, P};
unwrap_node_func(V) -> V.