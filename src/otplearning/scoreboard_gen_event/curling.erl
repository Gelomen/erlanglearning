%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 六月 2018 上午2:28
%%%-------------------------------------------------------------------
-module(curling).
-author("gelomenchen").

%% API
-export([start_link/2, set_team/3, add_point/3, next_round/1, join_feed/2, leave_feed/2, game_info/1]).


%% ============================================================================
%%                              外部 API
%% ============================================================================


start_link(TeamA, TeamB) ->
	{ok, Pid} = gen_event:start_link(),
	gen_event:add_handler(Pid, scoreboard, []),
	gen_event:add_handler(Pid, curling_accumulator, []),
	set_team(Pid, TeamA, TeamB),
	{ok, Pid}.

set_team(Pid, TeamA, TeamB) ->
	gen_event:notify(Pid, {set_teams, TeamA, TeamB}).

add_point(Pid, Team, N) ->
	gen_event:notify(Pid, {add_point, Team, N}).

game_info(Pid) ->
	gen_event:call(Pid, curling_accumulator, game_data).

next_round(Pid) ->
	gen_event:notify(Pid, next_round).

join_feed(Pid, ToPid) ->
	HandleId = {curling_feed, make_ref()},
	gen_event:add_handler(Pid, HandleId, [ToPid]),
	HandleId.

leave_feed(Pid, HandleId) ->
	gen_event:delete_handler(Pid, HandleId, leave_feed).

