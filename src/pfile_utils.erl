%%%-------------------------------------------------------------------
%%% @author Alexander Morozov aka ~ArchimeD~
%%% @copyright 2014, Alexander Morozov
%%% @doc
%%% String utils
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

-module(pfile_utils).
-author("Alexander Morozov aka ~ArchimeD~").

%% API
-export([normalize_string/1,
         truncate_id/2]).



%%--------------------------------------------------------------------
%% @doc
%% Removes the quotes from begin and end of the string, and replaces the
%% shadowed quotes with the customary one inside the string.
%% @end
%%--------------------------------------------------------------------

-spec normalize_string(string()) -> [string()].

normalize_string([]) ->
    [];

normalize_string([$" | TokenChars]) ->
    normalize_string(TokenChars, []).



-spec truncate_id(maybe_improper_list(),_) -> [any()].

truncate_id(TokenChars, _TokenLen) ->
    lists:takewhile(fun(E) when E == $\s;
                                E == $\r;
                                E == $\t;
                                E == $\n;
                                E == $= -> false;
                       (_) -> true end,
                    TokenChars).



-spec normalize_string(_,[any()]) -> [any()].

normalize_string([], Acc) ->
    lists:reverse(Acc);

normalize_string([$"], Acc) ->
    lists:reverse(Acc);

normalize_string([$\\ = Symb | Rest], Acc) ->
    {NewAcc, Tail} = shadow(Symb, Rest, Acc),
    normalize_string(Tail, NewAcc);

normalize_string([Symb | Rest], Acc) ->
    normalize_string(Rest, [Symb | Acc]).




-spec shadow(92,maybe_improper_list(),[any()]) -> {[any()],_}.

shadow(_Symb, [], Acc) ->
    {Acc, []};
shadow(_Symb, [$" = Next | Rest], Acc) ->
    {[Next | Acc], Rest};
shadow(Symb, [Next | Rest], Acc) ->
    {[Next, Symb | Acc], Rest}.
