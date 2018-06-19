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

flush() ->
  receive
    X -> [X|flush()]
  after 0 -> []
  end.

















