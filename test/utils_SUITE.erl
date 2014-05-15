%%%-------------------------------------------------------------------
%%% @author Alexander Morozov aka ~ArchimeD~
%%% @copyright 2014, Alexander Morozov
%%% @doc
%%% Tests for the pfile_utils module
%%% @end
%%%-------------------------------------------------------------------

-module(utils_SUITE).
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
     {"pfile_utils:truncate_id", ?_test(pfile_utils__truncate_id())},
     {"pfile_utils:normalize_string", ?_test(pfile_utils__normalize_string())}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Tests truncate_id function from pfile_utils
%% @end
%%--------------------------------------------------------------------

pfile_utils__truncate_id() ->
    ?assertEqual("id", pfile_utils:truncate_id("id", 0)),
    ?assertEqual("id", pfile_utils:truncate_id("id=", 0)),
    ?assertEqual("id", pfile_utils:truncate_id("id   =", 0)),
    ?assertEqual("id", pfile_utils:truncate_id("id\t=", 0)),
    ?assertEqual("id", pfile_utils:truncate_id("id\t  =\t", 0)),
    ?assertEqual("id", pfile_utils:truncate_id("id  \t=  ", 0)),
    ?assertEqual("id", pfile_utils:truncate_id("id\n=", 0)),
    ?assertEqual("id", pfile_utils:truncate_id("id\t\n\s=", 0)),
    ?assertEqual("id", pfile_utils:truncate_id("id\t\t\t\s\s\s\n\n\n=", 0)).
 

	
%%--------------------------------------------------------------------
%% @doc
%% Tests normalize_string function from pfile_utils
%% @end
%%--------------------------------------------------------------------

pfile_utils__normalize_string() ->
    ?assertError(_, pfile_utils:normalize_string("id")),
    ?assertEqual("id", pfile_utils:normalize_string("\"id\"")),
    ?assertEqual("\"id\"", pfile_utils:normalize_string("\"\\\"id\\\"\"")),
    ?assertEqual("id\"", pfile_utils:normalize_string("\"id\\\"\"")),
    ?assertEqual("id\\", pfile_utils:normalize_string("\"id\\")),
    ?assertEqual("\\nid", pfile_utils:normalize_string("\"\\nid\"")),
    ?assertEqual("'id'", pfile_utils:normalize_string("\"'id'\"")).
