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


Nonterminals root_symbol distinguished_symbol section_sequence first_section section section_ node node_ node__ leaf leaf_.

Terminals '{' '}' '=' 'token' 'string' ';' 'section_name'.

Rootsymbol root_symbol.

root_symbol ->
    distinguished_symbol : {config, '$1'}.

distinguished_symbol ->
    first_section : '$1'.
distinguished_symbol ->
    first_section section_sequence : ['$1' | '$2'].
distinguished_symbol ->
    section_sequence : ['$1'].

section_sequence ->
    section : ['$1'].
section_sequence ->
    section section_sequence : ['$1' | '$2'].


first_section ->
    node : ['$1'].
first_section ->
    leaf : ['$1'].
first_section ->
    node first_section : ['$1' | '$2'].
first_section ->
    leaf first_section : ['$1' | '$2'].



section ->
    'section_name' : {value('$1'), [[]]}.
section ->
    'section_name' section_ : {value('$1'), '$2'}.

section_ ->
    node : ['$1'].
section_ ->
    leaf : ['$1'].
section_ ->
    node section : ['$1' | '$2'].
section_ ->
    leaf section : ['$1' | '$2'].



node ->
    token '=' node_ : {value('$1'), '$2'}.
node ->
    node_ : '$1'.

node_ ->
    '{' '}' : [].
node_ ->
    '{' node__ '}' : '$2'.
node_ ->
    '{' node__ '}' ';' : '$2'.



node__ ->
    node : ['$1'].
node__ ->
    leaf : ['$1'].
node__ ->
    node node__ : ['$1' | '$2'].
node__ ->
    leaf node__ :  ['$1' | '$2'].



leaf ->
    token '=' leaf_ : {value('$1'), '$3'}.
leaf ->
    leaf_ : '$1'.


leaf_ ->
    ';' : null.
leaf_ ->
    token : value('$1').
leaf_ ->
    string : value('$1').
leaf_ ->
    token ';' : value('$1').
leaf_ ->
    string ';' : value('$1').


Erlang code.

value({Token, _Line}) ->
    Token;
value({_Token, Value, _Line}) ->
    Value.