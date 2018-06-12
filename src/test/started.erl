%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 六月 2018 上午9:43
%%%-------------------------------------------------------------------
-module(started).
-author("gelomenchen").

%% API
-export([add/2, hello/0, add_to_one/1, say/2, for/0, head/1, second/1, same/2, valid_time/1, old_enough/1, wrong_age/1, heh_fine/0, oh_god/1, help_me/1, insert/2, beach/1, guards_beach/1,
  what_am_i/1, fac/1, len/1, tail_fac/1, tail_len/1, duplicate/2, tail_duplicate/2, reverse/1, tail_reverse/1, sublist/2, tail_sublist/2, zip/2, tail_zip/2, lenient_zip/2]).


%% ============================== get started ==============================
add(X, Y) -> X + Y.

hello() ->
  io:format("Hello ~n").

add_to_one(X) ->
  hello(),
  add(X, 2).

say(male, X) ->
  io:format("Hi! Mr.~s", [X]);

say(female, X) ->
  io:format("Hi! Mrs.~s", [X]);

say(X, Y) ->
  io:format("HI! ~s, my little ~s~~ ~n", [Y, X]).

for() ->
  io:format("~s~n", [<<"Hello">>]),
  io:format("~p~n", [<<"Hello">>]),
  io:format("~~~n"),
  io:format("~f~n", [4.0]),
  io:format("~30f~n", [4.0]).

head([X | _]) ->
  X.

second([_, X | _]) ->
  X.

same(X, X) ->
  true;

same(_, _) ->
  false.

valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
  io:format("The Date tuple (~p) say today is: ~p/~p/~p,~n", [Date, Y, M, D]),
  io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time, H, Min, S]);

valid_time(_) ->
  io:format("Stop feeding me wrong data! ~n").

%% 卫语句 , 和 ; 的区别
old_enough(X) when X >=16, X =< 70 -> true;
old_enough(_) -> false.

wrong_age(X) when X < 16; X > 70 -> false;
wrong_age(_) -> true.


%% ============================== if 语句 ==============================
heh_fine() ->
  if 1 =:= 1 ->
    works
  end,
  if 1 =:=2; 1 =:= 1 ->
    works
  end,
  %% 这是错误的，因为这个 if 永远不会匹配上
  if 1 =:= 2, 1 =:= 1 ->
    fails
  end.

oh_god(X) ->
  if X =:= 2 -> might_succeed;
    true -> always_does  %% 这是 Erlang 的 else
  end.

help_me(Animail) ->
  Talk = if Animail == cat -> "meow";
           Animail == beef -> "bark";
           Animail == dog -> "bark";
           true -> "sfsfsfsfsfssd"
         end,
  {Animail, "says " ++ Talk ++ "!"}.


%% ============================== case ==============================
insert(X, []) ->
  [X];

insert(X, List) ->
  case lists:member(X, List) of
    true -> List;
    false -> [X | List]
  end.

beach(Temp) ->
  case Temp of
    {cel, X} when X >= 20, X =< 45 ->
      "favorable";
    {kel, X} when X >= 293, X =< 318 ->
      "scientifically favorable";
    {fah, X} when X >= 68, X =< 113 ->
      "favorable in the US";
    _ ->
      "avoid beach"
  end.

%% 和函数使用卫语句很类似
guards_beach({cel, X}) when X >= 20, X =< 45 ->
  "favorable";
guards_beach({kel, X}) when X >= 293, X =< 318 ->
  "scientifically favorable";
guards_beach({fah, X}) when X >= 68, X =< 113 ->
  "favorable in the US";
guards_beach(_) ->
  "avoid beach".


%% ============================== 判断类型 ==============================
what_am_i(X) when is_integer(X) ->
  X * 10;
what_am_i(X) when is_atom(X) ->
  X;
what_am_i(X) when is_binary(X) ->
  X;
what_am_i(X) when is_float(X) ->
  X;
what_am_i(_) ->
  "You are nothon! ~n".


%% ============================== 递归函数 ==============================
%% 阶乘
fac(0) -> 1;
fac(X) when X > 0 ->
  X * fac(X - 1).

%% 列表长度
len([]) -> 0;
len([_ | X]) -> 1 + len(X).

%% 尾递归实现，增加一个 临时变量参数，称为 累加器(accumulator)
tail_fac(X) -> tail_fac(X, 1).
tail_fac(0, Acc) -> Acc;
tail_fac(X, Acc) when X > 0 -> tail_fac(X - 1, X * Acc).

tail_len(X) -> tail_len(X, 0).
tail_len([], Acc) -> Acc;
tail_len([_ | X], Acc) -> tail_len(X, Acc + 1).


%% ----------------------------------------------

%% 复制 X 个元素到列表
duplicate(0, _) -> [];
duplicate(X, Y) when X > 0 -> [Y | duplicate(X - 1, Y)].
%% tail
tail_duplicate(X, Y) -> tail_duplicate(X, Y, []).
tail_duplicate(0, _, List) -> List;
tail_duplicate(X, Y, List) when X > 0 -> tail_duplicate(X - 1, Y, [Y | List]).

%% ----------------------------------------------

%% BIF 有这个函数，学完这个就改用 Lists:reverse/1
%% 颠倒列表
reverse([]) -> [];
reverse([X | Y]) -> reverse(Y) ++ [X].
%% tail
tail_reverse(List) -> tail_reverse(List, []).
tail_reverse([], Acc) -> Acc;
tail_reverse([X | Y], Acc) -> tail_reverse(Y, [X | Acc]).

%% ----------------------------------------------

%% 截取列表的前 N个
sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([X | Y], N) when N > 0 -> [X | sublist(Y, N - 1)].
%% tail
tail_sublist(L, N) -> lists:reverse(tail_sublist(L, N, [])).  %% 这里需要做个翻转
tail_sublist(_, 0, List) -> List;
tail_sublist([], _, List) -> List;
tail_sublist([X | Y], N, List) when N > 0 -> tail_sublist(Y, N - 1, [X | List]).

%% ----------------------------------------------

%% 拼合列表
zip([], []) -> [];
zip([X | Xx], [Y | Yy]) -> [{X, Y} | zip(Xx, Yy)].
%% tail
tail_zip([X | Xx], [Y | Yy]) -> lists:reverse(tail_zip([X | Xx], [Y | Yy], [])).
tail_zip([], _, List) -> List;
tail_zip(_, [], List) -> List;
tail_zip([X | Xx], [Y | Yy], List) -> tail_zip(Xx, Yy, [{X, Y} | List]).

%% --------

lenient_zip([], _) -> [];
lenient_zip(_, []) -> [];
lenient_zip([X | Xx], [Y | Yy]) -> [{X,Y} | lenient_zip(Xx, Yy)].







