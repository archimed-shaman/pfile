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
-export([normalize_string/2,
         truncate_id/2]).


normalize_string([_ | TokenChars], TokenLen) ->
    normalize_string(TokenChars, [], TokenLen - 1).



normalize_string([], Acc, _) ->
    lists:reverse(Acc);

normalize_string([$\\ = Symb | Rest], Acc, Counter) when Counter > 1 ->
    {NewAcc, Tail, NewCounter} = shadow(Symb, Rest, Acc, Counter - 1),
    normalize_string(Tail, NewAcc, NewCounter);

normalize_string([Symb | Rest], Acc, Counter) when Counter > 1 ->
    normalize_string(Rest, [Symb | Acc], Counter - 1);

normalize_string(_, Acc, _) ->
    lists:reverse(Acc).



shadow(_Symb, [], Acc, Counter) ->
    {Acc, [], Counter - 1};
shadow(_Symb, [$" = Next | Rest], Acc, Counter) ->
    {[Next | Acc], Rest, Counter - 1};
shadow(Symb, [Next | Rest], Acc, Counter) ->
    {[Next, Symb | Acc], Rest, Counter - 1}.



truncate_id(TokenChars, _TokenLen) ->
    lists:takewhile(fun(E) when E == $\s;
                                E == $\r;
                                E == $\t;
                                E == $\n;
                                E == $= -> false;
                       (E) -> true end,
                    TokenChars).