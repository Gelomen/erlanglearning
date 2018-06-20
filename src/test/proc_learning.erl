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








