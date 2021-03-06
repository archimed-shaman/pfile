%%%-------------------------------------------------------------------
%%% @author Alexander Morozov aka ~ArchimeD~
%%% @copyright 2014, Alexander Morozov
%%% @doc
%%% The main module with API functions
%%% @end
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

-module(pfile).
-author("Alexander Morozov aka ~ArchimeD~").

-type error_spec() :: {StringNo :: number(), pfile_parser,
                       Description :: [string()]}
                    | {StringNo :: number(), pfile_lexer,
                       Description :: term()}.

%% API

-export([parse/1]).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parses the input string.
%% @end
%%--------------------------------------------------------------------

-spec parse(String) -> Result when
      String :: string(),
      Result :: {ok, list()} |
                {error, error_spec()}.


parse(String) when is_list(String) ->
    parse_grammar(pfile_lexer:string(String)).



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Takes the input from lexer and passes it to parser
%% @end
%%--------------------------------------------------------------------

-spec parse_grammar(ParserOutput) -> Result when
      ParserOutput :: {error, Description, Line} |
                      {ok, Tokens, Lines},
      Description :: error_spec(),
      Line :: number(),
      Tokens :: number(),
      Line :: number(),
      Lines :: number(),
      Result :: {ok, list()} |
                {error, Description}.


parse_grammar({error, Description, _Lines} = _Error) ->
    {error, Description};

parse_grammar({ok, Tokens, _Lines}) ->
    pfile_parser:parse(Tokens).
