%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 六月 2018 上午10:36
%%%-------------------------------------------------------------------
-module(ops_test).
-author("gelomenchen").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).


%% ============================================================================
%%                              外部 API
%% ============================================================================

add_test() ->
    4 = ops:add(2, 2).

new_add_test() ->
    ?assertEqual(4, ops:add(2, 2)),
    ?assertEqual(3, ops:add(1, 2)),
    ?assert(is_number(ops:add(1, 2))),
    ?assertEqual(3, ops:add(1, 1)),
    ?assertError(badarith, 1/0).

add_test_() ->
    [test_them_types(),
        test_them_values(),
        ?_assertError(badarith, 1/0)].

test_them_types() ->
    ?_assert(is_number(ops:add(1, 2))).

test_them_values() ->
    [?_assertEqual(4, ops:add(2, 2)),
    ?_assertEqual(3, ops:add(1, 2)),
    ?_assertEqual(3, ops:add(1, 1))].

%% ============================================================================
%%                              内部 API
%% ============================================================================


%% ============================================================================
%%                             EUnit TEST
%% ============================================================================