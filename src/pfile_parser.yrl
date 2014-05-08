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


Nonterminals root_symbol pfile_grammar element section pair unary optionset options empty_value.

Terminals '{' '}' '=' 'id' 'value' ';' 'section_name'.

Rootsymbol root_symbol.

root_symbol ->
    pfile_grammar : {config, '$1'}.


pfile_grammar ->
    element : '$1'.
pfile_grammar ->
    element ';' : '$1'.
pfile_grammar ->
    section : '$1'.


section ->
    section_name : '$1'.


element ->
    pair : '$1'.
element ->
    unary : '$1'.


pair ->
    id '=' optionset : {value('$1'), '$3'}.
pair ->
    id '=' empty_value : {value('$1'), '$3'}.


unary ->
    value : value('$1').
unary ->
    optionset : '$1'.


optionset ->
    '{' '}' : [].
optionset ->
    '{' options '}' : '$2'.


options ->
    element ';' options : ['$1' | '$3'].
options ->
    element ';' : ['$1'].
options ->
    element : ['$1'].



empty_value ->
    value : value('$1').

empty_value ->
    '$empty' : nil.







Erlang code.

value({Token, _Line}) ->
    Token;
value({_Token, Value, _Line}) ->
    Value.