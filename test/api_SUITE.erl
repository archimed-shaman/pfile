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
     {"Simple config", ?_test(simple_config())},
     {"Simple config with semicolons", ?_test(simple_config_with_semicolons())},
     {"Quotes test", ?_test(quotes())},
     {"Single quotes test", ?_test(single_quotes())},
     {"Empty config", ?_test(empty())},
     {"Anonymous section", ?_test(anonymous_section())}
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

%%--------------------------------------------------------------------
%% @doc
%% Tests the simple configuration with semicolon dividors
%% @end
%%--------------------------------------------------------------------

simple_config_with_semicolons() ->
    Config = "[general]
              Key1 = \"value1\";
              key2 = \"value2\";
              [primary]
        primary1 = \"value1\";",
    ?assertEqual({ok, [{<<"general">>, [{<<"Key1">>, <<"value1">>},
                                        {<<"key2">>, <<"value2">>}]},
                       {<<"primary">>, [{<<"primary1">>, <<"value1">>}]}
                      ]}, pfile:parse(Config)).

%%--------------------------------------------------------------------
%% @doc
%% Tests quotes
%% @end
%%--------------------------------------------------------------------

quotes() ->
    Config = "key1 = value1 key2 = value2; key3=\"value 3\" key4 = value4",
    ?assertEqual({ok, [{[{<<"key1">>, <<"value1">>},
                         {<<"key2">>, <<"value2">>},
                         {<<"key3">>, <<"value 3">>},
                         {<<"key4">>, <<"value4">>}]}
                      ]}, pfile:parse(Config)).

%%--------------------------------------------------------------------
%% @doc
%% Tests single quotes
%% @end
%%--------------------------------------------------------------------

single_quotes() ->
    Config = "key1 = 'value1' key2 = 'value2'; key3=\"'value 3'\" key4 = 'value4'",
    ?assertEqual({ok, [{[{<<"key1">>, <<"'value1'">>},
                         {<<"key2">>, <<"'value2'">>},
                         {<<"key3">>, <<"'value 3'">>},
                         {<<"key4">>, <<"'value4'">>}]}
                      ]}, pfile:parse(Config)).

%%--------------------------------------------------------------------
%% @doc
%% Tests empty config
%% @end
%%--------------------------------------------------------------------

empty() ->
    Config = "",
    ?assertEqual({ok, []}, pfile:parse(Config)).


%%--------------------------------------------------------------------
%% @doc
%% Tests anonymous section
%% @end
%%--------------------------------------------------------------------

anonymous_section() ->
    Config = "Key1 = \"value1\";
              key2 = \"value2\";
              [primary]
        primary1 = \"value1\";",
    ?assertEqual({ok, [{[{<<"Key1">>, <<"value1">>},
                        {<<"key2">>, <<"value2">>}]},
                       {<<"primary">>, [{<<"primary1">>, <<"value1">>}]}
                      ]}, pfile:parse(Config)).
