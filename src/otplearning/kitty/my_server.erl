%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 六月 2018 下午11:27
%%%-------------------------------------------------------------------
-module(my_server).
-author("gelomenchen").

%% API
-export([start/2, start_link/2, call/2, cast/2,reply/2]).


%% 公共 API
start(Module, InitialState) ->
	spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
	spawn_link(fun() -> init(Module, InitialState) end).

call(Pid, Msg) ->
	Ref = erlang:monitor(process, Pid),
	%% 为了让 loop/2 匹配不同类型的消息，需要在这里新增 sync 原子突出同步调用
	Pid ! {sync, self(), Ref, Msg},
	receive
		{Ref, Reply} ->
			erlang:demonitor(Ref, [flush]),
			Reply;
		{'DOWN', Ref, process, pid, Reason} ->
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.

%% 为异步调用提供新函数
cast(Pid, Msg) ->
	Pid ! {async, Msg},
	ok.

reply({Pid, Ref}, Replay) ->
	Pid ! {Ref, Replay}.

%% 私有函数
init(Module, InitialState) ->
	loop(Module, Module:init(InitialState)).

loop(Module, State) ->
	receive
		{async, Msg} ->
			loop(Module, Module:handle_cast(Msg, State));
		{sync, Pid, Ref, Msg} ->
			loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
	end.
