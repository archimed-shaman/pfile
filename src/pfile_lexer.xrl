%%%-------------------------------------------------------------------
%%%
%%% Copyright (c) 2014 Alexander Morozov
%%%
%%% This file is part of pfile.
%%%
%%% pfile is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% pfile is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with pfile.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

Definitions.

LWS = [\s\r\t\n]
STRING = [\s*&@%^~|A-Za-z0-9_.?<>/'(),+:!\x80-\xff-]
TOKEN = [*&@%^~|A-Za-z0-9_.?<>/'(),+:!\x80-\xff-]
SECTION = [A-Za-z0-9_.-]
ID = [A-Za-z0-9_.-]

Rules.


\[{SECTION}+\] : A = strip(TokenChars,TokenLen), {token, {section_name, A, TokenLine}}.
[\{\};=] : {token,{list_to_atom(TokenChars), TokenLine}}.


\"({STRING}|\x5c\x22|{LWS})+\" : S = normalize_string(TokenChars,TokenLen), {token, {value, S, TokenLine}}.

{ID}+{LWS}*= : {token, {id, truncate_id(TokenChars, TokenLen), TokenLine} ,[$=]}.

{TOKEN}+ : {token, {value, TokenChars, TokenLine}}.

#.* : skip_token.

{LWS}+ : skip_token.


Erlang code.


strip(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).

normalize_string(TokenChars, _TokenLen) ->
    pfile_utils:normalize_string(TokenChars).

truncate_id(TokenChars, TokenLen) ->
    pfile_utils:truncate_id(TokenChars, TokenLen).


