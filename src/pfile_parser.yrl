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


Nonterminals root_symbol section_sequence sections element section noname_section
             pair unary optionset option empty_value semicolon.

Terminals '{' '}' '=' 'id' 'value' ';' 'section_name'.

Rootsymbol root_symbol.

root_symbol ->
    section_sequence : '$1'.


section ->
    section_name option : {value('$1'), '$2'}.
section ->
    section_name : {value('$1'), []}.


noname_section ->
    option : {'$1'}.


section_sequence ->
    noname_section sections : ['$1' | '$2'].
section_sequence ->
    sections : '$1'.

sections ->
    section sections : ['$1' | '$2'].
sections ->
    '$empty' : [].


pair ->
    id '=' optionset : {value('$1'), '$3'}.
pair ->
    id '=' empty_value : {value('$1'), '$3'}.


unary ->
    value : value('$1').
unary ->
    optionset : '$1'.


option ->
    element semicolon option : ['$1' | '$3'].
option ->
    element semicolon : ['$1'].



element ->
    pair : '$1'.
element ->
    unary : '$1'.


optionset ->
    '{' '}' : [].
optionset ->
    '{' option '}' : '$2'.


empty_value ->
    value : value('$1').
empty_value ->
    '$empty' : nil.


semicolon ->
    ';' semicolon : '$1'.
semicolon  ->
    '$empty' : nil.


Erlang code.

value({Token, _Line}) ->
    list_to_binary(Token);
value({_Token, Value, _Line}) ->
    list_to_binary(Value).