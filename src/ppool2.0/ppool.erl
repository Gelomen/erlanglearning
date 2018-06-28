%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 六月 2018 下午4:05
%%%-------------------------------------------------------------------
-module(ppool).
-author("gelomenchen").
-behavior(application).

%% API
-export([start/2, stop/1, start_pool/3, stop_pool/1, run/2, async_queue/2, sync_queue/2]).


%% ============================================================================
%%                              外部 API
%% ============================================================================

start(normal, _Args) ->
    ppool_supersup:start_link().

stop(_State) ->
    ok.

start_pool(Name, Limit, {M, F, A}) ->
    ppool_supersup:start_pool(Name, Limit, {M, F, A}).

stop_pool(Name) ->
    ppool_supersup:stop_pool(Name).

run(Name, Args) ->
    ppool_serv:run(Name, Args).

async_queue(Name, Args) ->
    ppool_serv:async_queue(Name, Args).

sync_queue(Name, Args) ->
    ppool_serv:sync_queue(Name, Args).

%% ============================================================================
%%                              内部 API
%% ============================================================================


%% ============================================================================
%%                             EUnit TEST
%% ============================================================================