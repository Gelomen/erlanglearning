%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 六月 2018 上午2:02
%%%-------------------------------------------------------------------
-module(scoreboard).
-author("gelomenchen").
-behavior(gen_event).

%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).


%% ============================================================================
%%                              外部 API
%% ============================================================================

init([]) -> {ok, []}.

handle_event({set_teams, TeamA, TeamB}, State) ->
	scoreboard_hw:set_teams(TeamA, TeamB),
	{ok, State};

handle_event({add_point, Team, N}, State) ->
	[scoreboard_hw:add_point(Team) || _ <- lists:seq(1, N)],
	{ok, State};

handle_event(next_round, State) ->
	scoreboard_hw:next_round(),
	{ok, State};

handle_event(_, State) ->
	{ok, State}.

handle_call(_, State) ->
	{ok, ok, State}.

handle_info(_, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) -> ok.














%% ============================================================================
%%                              内部 API
%% ============================================================================


%% ============================================================================
%%                             EUnit TEST
%% ============================================================================