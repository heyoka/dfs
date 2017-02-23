% Lexer for Dataflow framework
Definitions.

If              = (if)
Lambda          = lambda:(\s|\t)*
StreamId        = [\'][0-9]\.[0-9]{3}\.[0-9a-zA-Z]*[\']
Reference       = \"(\"\"|[^\"\n]|[\.])*\"
Operator        = (\+|-|\*|\/|==|!=|<|<=|>|>=|=~|!~|!|AND|OR|div|rem)
Duration        = [1-9]+[0-9]*(m|ms|s|h|d|w)
Identifier      = [a-z_][0-9a-zA-Z_\.]*
Int             = (\+|-)?[1-9]+[0-9]*
Float           = (\+|-)?[0-9]+[\.]+[0-9]+
%Float           = (\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)?
Digit           = [0-9]+
WhiteSpace      = ([\000-\s]|%.*)
String          = [\'][A-Za-z_0-9_\.\%&#\+-\/!~=~><=~\*]*[\']
Text            = [\'][A-Za-z_0-9_\.\!/\\%&#+-~\*\s]*[\']
Regex           = [\?][A-Za-z_]*[\?]
True            = (T|t)(R|r)(U|u)(E|e)
False           = (F|f)(A|a)(L|l)(S|s)(E|e)
Node            = \|
UserNode        = \@
Slash           = [\\\"]

Rules.

{If}            :   {token, {'if', TokenLine, list_to_atom(TokenChars)}}.
var             :   {token, {var, TokenLine, list_to_atom(TokenChars)}}.
def             :   {token, {def, TokenLine, list_to_atom(TokenChars)}}.
{Operator}      :   {token, {operator,TokenLine,list_to_atom(TokenChars)}}.
{StreamId}      :   {token, {stream_id, TokenLine, list_to_binary(TokenChars)}}.
{Lambda}        :   {token, {lambda, TokenLine, list_to_binary(lists:sublist(TokenChars,9,length(TokenChars)))}}.
{UserNode}      :   {token, {user_node, TokenLine, 'user_node'}}.
{Node}          :   {token, {node, TokenLine, 'node'}}.
{Duration}      :   {token, {duration,TokenLine,TokenChars}}.
{Identifier}    :   {token, {identifier,TokenLine,list_to_binary(TokenChars)}}.
{Reference}     :   {token, {reference,TokenLine,unquote(TokenChars)}}.
%{Number}       :   {token, {number,TokenLine,list_to_float(TokenChars)}}.
{Float}         :   {token, {float,TokenLine,list_to_float(TokenChars)}}.
{Int}           :   {token, {int,TokenLine,list_to_integer(TokenChars)}}.
{String}        :   {token, {string,TokenLine, list_to_binary(strip(TokenChars, length(TokenChars)))}}.
{Text}          :   {token, {text,TokenLine,list_to_binary(strip(TokenChars, length(TokenChars)))}}.
{Regex}         :   {token, {regex,TokenLine,prep_regex(TokenChars)}}.
{True}          :   {token, {bool,TokenLine,true}}.
{False}         :   {token, {bool,TokenLine,false}}.
%{Digit}        :   {token, {digit,TokenLine,TokenChars}}.
[(),\.=:]       :   {token, {list_to_atom(TokenChars),TokenLine}}.
\%.*            :   skip_token.
{WhiteSpace}+   :   skip_token.
{Slash}         :   skip_token.

Erlang code.

-export([reserved_word/1]).

reserved_word('var') -> true.

strip(TokenChars,TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).
strip_ref(TokenChars,TokenLen) -> lists:sublist(TokenChars, 3, TokenLen - 3).
unquote(TokenChars) -> binary:replace(list_to_binary(TokenChars),<<"\"">>, <<>>, [global]).
prep_regex(TokenChars) -> binary:replace(list_to_binary(TokenChars),<<"?">>, <<>>, [global]).