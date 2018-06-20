%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 六月 2018 下午3:35
%%%-------------------------------------------------------------------
-module(proc_learning).
-author("gelomenchen").
-compile([debug_info, export_all]).

%% API
-export([]).


% ============================== 定义进程状态 ==============================

% 模拟冰箱存入和取出东西
fridge(FoodList) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge([Food | FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
          true ->
              From ! {self(), {ok, Food}},
              fridge(lists:delete(Food, FoodList));
          false ->
              From ! {self(), not_found},
              fridge(FoodList)
      end;
      terminate ->
        ok
  end.

% 隐藏消息实现
store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  end.

take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  end.

% 隐藏进程启动部分
start(FoodList) ->
  spawn(?MODULE, fridge, [FoodList]).

% ============ 超时
store_delay(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

take_delay(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

sleep(T) ->
  receive
  after T -> ok
  end.

% 获取当前进程所有信息，并加入到列表
flush() ->
  receive
    X -> [X|flush()]
  after 0 -> []
  end.

% 按等级加入信息
important() ->
  receive
    {Priority, Msg} when  Priority > 10 ->
      [Msg | important()]
  after 0 ->
    normal()
  end.

normal() ->
  receive
    {_, Msg} ->
      [Msg | normal()]
  after 0 ->
    []
  end.



% ============================== 错误与进程 ==============================

% ===== 链接

my_proc() ->
  timer:sleep(5000),
  exit(reason).

% ===== 大型进程组

chain(0) ->
  receive
    _ -> ok
  after 2000 ->
    exit("chain dies here")
  end;
chain(N) ->
  Pid = spawn(fun() -> chain(N - 1) end),
  link(Pid),
  receive
    _ -> ok
  end.

% ===== 命名进程

start_critic() ->
  spawn(?MODULE, critic, []).

judge(Pid, Band, Album) ->
  Pid ! {self(), {Band, Album}},
  receive
    {Pid, Criticism} -> Criticism
  after 2000 ->
    timeout
  end.

critic() ->
  receive
    {From, {"A", "B"}} ->
      From ! {self(), "AB"};
    {From, {"C", "D"}} ->
      From ! {self(), "CD"};
    {From, {"E", "F"}} ->
      From ! {self(), "EF"};
    {From, {_Band, _Album}} ->
      From ! {self(), "bla bla bla"}
  end,
  critic().

% 让 critic 自动复活
start_critic_reborn() ->
  spawn(?MODULE, reborn, []).

reborn() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic_ref, []),
  register(critic, Pid),
  receive
    {'EXIT', Pid, normal} -> ok;
    {'EXIT', Pid, shutdown} -> ok;
    {'EXIT', Pid, _} -> reborn()
  end.

judge(Band, Album) ->
  Ref = make_ref(),
  critic ! {self(), Ref, {Band, Album}},
  receive
    {Ref, Criticism} -> Criticism
  after 2000 ->
    timeout
  end.

critic_ref() ->
  receive
    {From, Ref, {"A", "B"}} ->
      From ! {Ref, "AB"};
    {From, Ref, {"C", "D"}} ->
      From ! {Ref, "CD"};
    {From, Ref, {"E", "F"}} ->
      From ! {Ref, "EF"};
    {From, Ref, {_Band, _Album}} ->
      From ! {Ref, "bla bla bla"}
  end,
  critic_ref().