
Definitions.

ATOM_CHAR = [~|a-zA-Z@\^\-_*!?/0-9#<>`$&^=+%:;,.\(\)\{\}\[\]]

UNQUOTED_ATOM_CHAR = [a-zA-Z_0-9@,]
UNQUOTED_ATOM_START = [a-z_]

Rules.

\( :
   {token, {'(', TokenLine}}.

\) :
   {token, {')', TokenLine }}.

[[] :
   {token, {'[', TokenLine}}.

[]] :
   {token, {']', TokenLine}}.

\. :
   {token, {cons, TokenLine}}.

(\s|\r|\t|\n)+ :
  skip_token.

"([\\]"|[^"])*" :
  {token, {string, TokenLine, drop_first_and_last(TokenChars)}}.

{UNQUOTED_ATOM_START}{UNQUOTED_ATOM_CHAR}* :
  {token, {symbol, TokenLine, list_to_atom(TokenChars)}}.

'{ATOM_CHAR}+' :
  {token, {symbol, TokenLine, list_to_atom(drop_first_and_last(TokenChars))}}.

[0-9]+ :
  {token, {number, TokenLine, list_to_integer(TokenChars)}}.
-[0-9]+ :
  {token, {number, TokenLine, (-1) * list_to_integer(tl(TokenChars))}}.

Erlang code.

drop_first_and_last([_|Rest]) ->
   lists:reverse(tl(lists:reverse(Rest))).
