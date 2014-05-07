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


Nonterminals document block blocks entries entry word equation.

Terminals '{' '}' '=' 'term' ';' 'section'.

Rootsymbol document.


document ->
    blocks : '$1'.
document ->
    section blocks : {value('$1'), '$2'}.

blocks ->
    block : '$1'.
blocks ->
    block blocks : ['$1' | ['$2']].

block ->
    '{' '}' : [].
block ->
    '{' entries '}' : '$2'.

entries ->
    entry : ['$1'].
entries ->
    entry ';' entries: ['$1' | '$3'].

entry ->
    word : '$1'.
entry ->
    block : '$1'.
entry ->
    equation : '$1'.

equation ->
    word '=' word : {'$1', '$3'}.
equation ->
    word '=' blocks : {'$1', '$3'}.

word ->
    'term' : value('$1').


Erlang code.

value({Token, _Line}) ->
    Token;
value({_Token, Value, _Line}) ->
    Value.