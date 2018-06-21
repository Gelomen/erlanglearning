%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 六月 2018 上午10:11
%%%-------------------------------------------------------------------
-module(event).
-author("gelomenchen").
-compile([debug_info, export_all]).
-record(state, {server, name="", to_go=0}).

%% API
-export([]).


normalize(N) ->
    Limit = 49 * 24 * 60 * 60,    %% 因为 erlang 的超时值最大为50天（不包含50）
    [N rem Limit | lists:duplicate(N rem Limit, Limit)].


loop(S = #state{server=Server, to_go = [H | T]}) ->
  receive
    {Server, Ref, cancel} ->
        Server ! {Ref, ok}
  after H * 1000 ->   %% * 1000 是为了将秒转为毫秒，也可以使用 timer:seconds/1
      if T =:= [] ->
            Server ! {done, S#state.name};
         T =/= [] ->
            loop(S#state{to_go = T})
      end
  end.
