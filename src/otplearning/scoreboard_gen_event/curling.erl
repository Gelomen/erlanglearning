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
-export([start_link/2, set_team/3, add_point/3, next_round/1]).


%% ============================================================================
%%                              外部 API
%% ============================================================================


start_link(TeamA, TeamB) ->
	{ok, Pid} = gen_event:start_link(),
	gen_event:add_handler(Pid, scoreboard, []),
	set_team(Pid, TeamA, TeamB),
	{ok, Pid}.

set_team(Pid, TeamA, TeamB) ->
	gen_event:notify(Pid, {set_team, TeamA, TeamB}).

add_point(Pid, Team, N) ->
	gen_event:notify(Pid, {add_point, Team, N}).

next_round(Pid) ->
	gen_event:notify(Pid, next_round).