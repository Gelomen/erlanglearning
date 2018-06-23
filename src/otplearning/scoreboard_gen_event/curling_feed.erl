%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 六月 2018 下午4:46
%%%-------------------------------------------------------------------
-module(curling_feed).
-author("gelomenchen").
-behavior(gen_event).

%% API
-export([init/1, handle_event/2, handle_call/2, code_change/3, terminiate/2]).


%% ============================================================================
%%                              外部 API
%% ============================================================================


init([Pid]) -> {ok, Pid}.

handle_event(Event, Pid) ->
	Pid ! {curling_feed, Event},
	{ok, Pid}.

handle_call(_, State) ->
	{ok, ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminiate(_Reason, _State) -> ok.