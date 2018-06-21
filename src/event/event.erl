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
%%-record(state, {server, name = "", to_go = 0}).
-record(state, {clients,  %% 客户端列表
                events}). %% #event{} 记录列表

-record(event, {name="", description="", pid, timeout={{1970,1,1},{0,0,0}}}).

%% API
-export([]).


start(EventName, DateTime) ->
  spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
  spawn_link(?MODULE, init, [self(), EventName, DateTime]).

init() ->
  loop(#state{events = orddict:new(), clients = orddict:new()}).


%% erlang 的超时值最多为50天，通过以下函数，将时间等分成多个49天
normalize(N) ->
  Limit = 49 * 24 * 60 * 60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].


%% 主循环
loop(S = #state{}) ->
  receive
%%    %%  处理取消
%%    {Server, Ref, cancel} ->
%%        Server ! {Ref, ok};

    %% ===== 客户端主动给服务器发送的消息
    %% 处理订阅
    {Pid, MsgRef, {subscribe, Client}} ->
        Ref = erlang:monitor(process, Client),
        NewClients = orddict:store(Ref, Client, S#state.clients),
        Pid ! {MsgRef, ok},
        loop(S#state{clients = NewClients});
    %% 处理新增事件
    {Pid, MsgRef, {add, Name, Description, Timeout}} ->
        case valid_datetime(Timeout) of
            true ->
                EventPid = event:start_link(Name, Timeout),
                NewEvents = orddict:store(Name, #event{name = Name, description = Description, pid = EventPid, timeout = Timeout}, S#state.events),
                Pid ! {MsgRef, ok},
                loop(S#state{events = NewEvents});
            false ->
                Pid ! {MsgRef, {error, bad_timeout}},
                loop(S)
        end;

    %% ===== 服务端处理事件消息
    %% 事件超时
    {done, Name} ->
        case orddict:find(Name, S#state.events) of
          {ok, E} ->
              send_to_clients({done, E#event.name, E#event.description}, S#state.clients),
              NewEvents = orddict:erase(Name, S#state.events),
              loop(S#state{events = NewEvents});
          error -> loop(S)
        end;
    %%  处理取消
    {Pid, MsgRef, {cancel, Name}} ->
      Events = case orddict:find(Name, S#state.events) of
                 {ok, E} ->
                   event:cancel(E#event.pid),
                   orddict:erase(Name, S#state.events);
                 error ->
                   S#state.events
               end,
      Pid ! {MsgRef, ok},
      loop(S#state{events = Events});

    shutdown ->
        exit(shutdown);
    {'DOWN', Ref, process, _Pid, _Reason} ->
        loop(S#state{clients = orddict:erase(Ref, S#state.clients)});
    code_change ->
        ?MODULE:loop(S);
    Unknown ->
        io:format("Unknown message: ~p~n", [Unknown]),
        loop(S)

%%  after T * 1000 ->   %% * 1000 是为了将秒转为毫秒，也可以调用 timer:seconds/1
%%    if Next =:= [] ->
%%          Server ! {done, S#state.name};
%%       Next =/= [] ->
%%          loop(S#state{to_go = Next})
%%    end
  end.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

%% 主动取消事件
cancel(Pid) ->
  %% 设置监控器，以免进程已经死亡
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cencel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.


%% 判断日期和时间是否合法
valid_datetime({Date, Time}) ->
  try
      calendar:valid_date(Date) andalso valid_time(Time)
  catch
      error: function_clause ->
        false
  end;
valid_datetime(_) ->
  false.

valid_time({H, M, S}) -> valid_time(H, M, S).
valid_time(H, M, S) when H >= 0, H < 24,
                         M >= 0, M < 60,
                         S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.


%% 根据输入的日期和时间，获得与当前时间的差值
time_to_go(TimeOut = {{_,_,_}, {_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) - calendar:datetime_to_gregorian_seconds(Now),
  Secs = if ToGo > 0 -> ToGo;
            ToGo =< 0 -> 0
         end,
  normalize(Secs).