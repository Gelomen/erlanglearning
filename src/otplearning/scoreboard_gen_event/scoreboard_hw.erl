%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 六月 2018 上午1:55
%%%-------------------------------------------------------------------
-module(scoreboard_hw).
-author("gelomenchen").

%% API
-export([set_teams/2, next_round/0, add_point/1, reset_board/0]).

%% 这是一个“哑”模块，只是用于代替真正的硬件控制器要做的事情。
%% 一个真实的硬件控制器要能保存某些状态并且确保一切工作正常，但是这个模块无需关心这些


set_teams(TeamA, TeamB) ->
	io:format("Scoreboard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).

next_round() ->
	io:format("Scoreboard: round over ~n").

add_point(Team) ->
	io:format("Scoreboard: increased score of team ~s by 1~n", [Team]).

reset_board() ->
	io:format("Scoreboard: All teams are undefined and all scores are 0~n").


