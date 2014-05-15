%%%-------------------------------------------------------------------
%%% @author Alexander Morozov aka ~ArchimeD~
%%% @copyright 2014, Alexander Morozov
%%% @doc
%%% Tests for the API of the main module pfile.erl
%%% @end
%%%-------------------------------------------------------------------

-module(api_SUITE).
-author("Alexander Morozov aka ~ArchimeD~").

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").



%%--------------------------------------------------------------------
%% @doc
%% A "fixture" to run tests
%% @end
%%--------------------------------------------------------------------

setup_test_()->
    Suite = { foreach, local, fun setup/0, fun cleanup/1, tests() },
    Suite.



%%--------------------------------------------------------------------
%% @doc
%% Initializes tests
%% @end
%%--------------------------------------------------------------------

setup()->
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Cleans up the tests
%% @end
%%--------------------------------------------------------------------

cleanup(_) ->
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Returns the test set
%% @end
%%--------------------------------------------------------------------

tests() ->
    [
     {"Simple config", ?_test(simple_config())}
    ].



%%--------------------------------------------------------------------
%% @doc
%% Tests the simple configuration
%% @end
%%--------------------------------------------------------------------

simple_config() ->
    Config = "[general]
              Key1 = \"value1\"
              key2 = \"value2\"
              [primary]
              primary1 = \"value1\"",
    ?assertEqual({ok, [{<<"general">>, [{<<"Key1">>, <<"value1">>},
					{<<"key2">>, <<"value2">>}]},
		       {<<"primary">>, [{<<"primary1">>, <<"value1">>}]}
		      ]}, pfile:parse(Config)).

