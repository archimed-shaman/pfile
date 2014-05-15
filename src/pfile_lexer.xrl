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

%%%===================================================================
%%% Definitions
%%%===================================================================

Definitions.

LWS = [\s\r\t\n]
TOKEN = [*&@%^~|A-Za-z0-9_.?<>/'(),+:!\x80-\xff-]
SECTION = [A-Za-z0-9_.-]
ID = [A-Za-z0-9_.-]

%%%===================================================================
%%% Rules
%%%===================================================================

Rules.

%% section name
\[{SECTION}+\] : A = strip(TokenChars,TokenLen), {token, {section_name, A, TokenLine}}.

%% symbols {};=, leave them as is
[\{\};=] : {token,{list_to_atom(TokenChars), TokenLine}}.

%% quoted string with possible shadowed quotes - \"
\"(\\.|[^"])*\" : S = normalize_string(TokenChars,TokenLen), {token, {value, S, TokenLine}}.

%% key with equation symbol. takes the word and returns '=' symbol to the stream
{ID}+{LWS}*= : {token, {id, truncate_id(TokenChars, TokenLen), TokenLine} ,[$=]}.

%% all ther values are interpreted, as values
{TOKEN}+ : {token, {value, TokenChars, TokenLine}}.

%% comments
#.* : skip_token.

%% spaces, tabulars, line wraps, just skip them
{LWS}+ : skip_token.

%%%===================================================================
%%% Erlang code
%%%===================================================================

Erlang code.



strip(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).



normalize_string(TokenChars, _TokenLen) ->
    pfile_utils:normalize_string(TokenChars).



truncate_id(TokenChars, TokenLen) ->
    pfile_utils:truncate_id(TokenChars, TokenLen).


