%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 六月 2018 下午5:20
%%%-------------------------------------------------------------------
-module(curling_accumulator).
-author("gelomenchen").
-behavior(gen_event).
-record(state, {teams = orddict:new(), round=0}).

%% API
-export([init/1, handle_call/2, handle_event/2, handle_info/2, code_change/3, terminate/2]).


%% ============================================================================
%%                              外部 API
%% ============================================================================

init([]) -> {ok, #state{}}.

handle_event({set_teams, TeamA, TeamB}, S = #state{teams = T}) ->
	Teams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, T)),
	{ok, S#state{teams = Teams}};
handle_event({add_point, Team, N}, S = #state{teams = T}) ->
	Teams = orddict:update_counter(Team, N, T),
	{ok, S#state{teams = Teams}};
handle_event(next_round, S = #state{}) ->
	{ok, S#state{round = S#state.round + 1}};
handle_event(_Event, State = #state{}) ->
	{ok, State}.

handle_call(game_data, S = #state{teams = T, round = R}) ->
	{ok, {orddict:to_list(T), {round, R}}, S};
handle_call(_Event, State) ->
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