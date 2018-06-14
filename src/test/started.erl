%%-------------------------------------------------------------------
%% @author gelomenchen
%% @copyright (C) 2018, <COMPANY>
%% @doc
%%
%% @end
%% Created : 12. 六月 2018 上午9:43
%%-------------------------------------------------------------------
-module(started).
-author("gelomenchen").
-define(sub(X, Y), X - Y).
-compile([export_all]).   % 只为调试方便

% API
-export([add/2, hello/0, add_to_one/1]).


% ============================== get started ==============================
add(X, Y) -> X + Y.

hello() ->
  io:format("Hello ~n"),
  ?sub(1, 2).

add_to_one(X) ->
  hello(),
  add(X, 2).

% 判断宏 DEBUGMODE 是否被定义，若定义了则定义另一个宏 DEBUG，打印对应的信息，否则只会打印原子 ok
-ifdef(DEBUGMODE).
-define(DEBUG(S), io:format("dgb: " ++S)).
-else.
-define(DEBUG(S), ok).
-endif.

% 判断宏 TEST 是否被定义，若定义了才会定义 my_test_func 测试函数
-ifdef(TEST).
my_test_func() ->
  hello().
-endif.

say(male, X) ->
  io:format("Hi! Mr.~s", [X]);

say(female, X) ->
  io:format("Hi! Mrs.~s", [X]);

say(X, Y) ->
  io:format("HI! ~s, my little ~s~~ ~n", [Y, X]).

forM() ->
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

% 卫语句 , 和 ; 的区别
old_enough(X) when X >=16, X =< 70 -> true;
old_enough(_) -> false.

wrong_age(X) when X < 16; X > 70 -> false;
wrong_age(_) -> true.


% ============================== if 语句 ==============================
heh_fine() ->
  if 1 =:= 1 ->
    works
  end,
  if 1 =:=2; 1 =:= 1 ->
    works
  end,
  % 这是错误的，因为这个 if 永远不会匹配上
  if 1 =:= 2, 1 =:= 1 ->
    fails
  end.

oh_god(X) ->
  if X =:= 2 -> might_succeed;
    true -> always_does  % 这是 Erlang 的 else
  end.

help_me(Animail) ->
  Talk = if Animail == cat -> "meow";
           Animail == beef -> "bark";
           Animail == dog -> "bark";
           true -> "sfsfsfsfsfssd"
         end,
  {Animail, "says " ++ Talk ++ "!"}.


% ============================== case ==============================
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

% 和函数使用卫语句很类似
guards_beach({cel, X}) when X >= 20, X =< 45 ->
  "favorable";
guards_beach({kel, X}) when X >= 293, X =< 318 ->
  "scientifically favorable";
guards_beach({fah, X}) when X >= 68, X =< 113 ->
  "favorable in the US";
guards_beach(_) ->
  "avoid beach".


% ============================== 判断类型 ==============================
what_am_i(X) when is_integer(X) ->
  X * 10;
what_am_i(X) when is_atom(X) ->
  X;
what_am_i(X) when is_binary(X) ->
  X;
what_am_i(X) when is_float(X) ->
  X;
what_am_i(_) ->
  "You are nothing! ~n".


% ============================== 递归函数 ==============================
% 阶乘
fac(0) -> 1;
fac(X) when X > 0 ->
  X * fac(X - 1).

% 列表长度
len([]) -> 0;
len([_ | X]) -> 1 + len(X).

% 尾递归实现，增加一个 临时变量参数，称为 累加器(accumulator)
tail_fac(X) -> tail_fac(X, 1).
tail_fac(0, Acc) -> Acc;
tail_fac(X, Acc) when X > 0 -> tail_fac(X - 1, X * Acc).

tail_len(X) -> tail_len(X, 0).
tail_len([], Acc) -> Acc;
tail_len([_ | X], Acc) -> tail_len(X, Acc + 1).


% ----------------------------------------------

% 复制 X 个元素到列表
duplicate(0, _) -> [];
duplicate(X, Y) when X > 0 -> [Y | duplicate(X - 1, Y)].
% tail
tail_duplicate(X, Y) -> tail_duplicate(X, Y, []).
tail_duplicate(0, _, List) -> List;
tail_duplicate(X, Y, List) when X > 0 -> tail_duplicate(X - 1, Y, [Y | List]).

% ----------------------------------------------

% BIF 有这个函数，学完这个就改用 Lists:reverse/1
% 颠倒列表
reverse([]) -> [];
reverse([X | Y]) -> reverse(Y) ++ [X].
% tail
tail_reverse(List) -> tail_reverse(List, []).
tail_reverse([], Acc) -> Acc;
tail_reverse([X | Y], Acc) -> tail_reverse(Y, [X | Acc]).

% ----------------------------------------------

% 截取列表的前 N个
sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([X | Y], N) when N > 0 -> [X | sublist(Y, N - 1)].
% tail
tail_sublist(L, N) -> lists:reverse(tail_sublist(L, N, [])).  % 这里需要做个翻转
tail_sublist(_, 0, List) -> List;
tail_sublist([], _, List) -> List;
tail_sublist([X | Y], N, List) when N > 0 -> tail_sublist(Y, N - 1, [X | List]).

% ----------------------------------------------

% 拼合列表
zip([], []) -> [];
zip([X | Xx], [Y | Yy]) -> [{X, Y} | zip(Xx, Yy)].
% tail
tail_zip([], []) -> [];
tail_zip([X | Xx], [Y | Yy]) -> lists:reverse(tail_zip([X | Xx], [Y | Yy], [])).
tail_zip([], [], List) -> List;
tail_zip([X | Xx], [Y | Yy], List) -> tail_zip(Xx, Yy, [{X, Y} | List]).

% --------

lenient_zip([], _) -> [];
lenient_zip(_, []) -> [];
lenient_zip([X | Xx], [Y | Yy]) -> [{X,Y} | lenient_zip(Xx, Yy)].

% tail
tail_lenient_zip([], _) -> [];
tail_lenient_zip(_, []) -> [];
tail_lenient_zip([X | Xx], [Y | Yy]) -> lists:reverse(tail_lenient_zip([X | Xx], [Y | Yy], [])).
tail_lenient_zip([], _, List) -> List;
tail_lenient_zip(_, [], List) -> List;
tail_lenient_zip([X | Xx], [Y | Yy], List) -> tail_lenient_zip(Xx, Yy, [{X, Y} | List]).

% ----------------------------------------------

% 快速排序列表  教学例子而已，列表排序用 lists:sort/1
quick_sort([]) -> [];
quick_sort([X | Y]) ->
  {Smaller, Larger} = partition(X, Y, [], []),
  quick_sort(Smaller) ++ [X] ++ quick_sort(Larger).
% partition/4 将列表分割成大小两部分，然后存入元组 {Smaller, Larger}
partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(X, [H | T], Smaller, Larger) ->
  if H =< X -> partition(X, T, [H | Smaller], Larger);
     H > X -> partition(X, T, Smaller, [H | Larger])
  end.

% ----------------------------------------------

% 树，元组包含一个健、一个键值，还有另外两个结点，其中一个结点比包含它的结点的健大，另一个比包含它的结点的健小
% 空结点也是树的根结点(root)
empty() -> {node, "nil"}.

% 添加结点
insert(Key, Val, {node, "nil"}) ->
  {node, {Key, Val, {node, "nil"}, {node, "nil"}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
  {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
  {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
  {node, {Key, Val, Smaller, Larger}}.

% 查询结点
lookup(_, {node, "nil"}) ->
  undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
  {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
  lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
  lookup(Key, Larger).


% ============================== 高阶函数 ==============================

% 但这种方式需要把函数都放在同一个模块中，还要导出，然后编译后才能用，不太实用
map(_, []) -> [];
map(F, [H | T]) -> [F(H) | map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.


% =========== 匿名函数

% 函数内的所有变量及其值都是该函数作用域的一部分，比如 `base(A) -> B = A + 1.`
% 函数 `base/1` 中任何地方都可以引用 `A` 和 `B`，甚至匿名函数也可以调用它们
base(A) ->
  B = A + 1,
  F = fun() -> A * B end,
  F().

% 但继承是单向的，在匿名函数里定义变量 `C`，在匿名函数外调用 `C`，这样是错误的
% 编译会直接报错
%%base2(A) ->
%%  B = A + 1,
%%  F = fun() -> C = A * B end,
%%  F(),
%%  C.


% 不管匿名函数在哪，被继承的作用域会一直跟着它，即使把这个匿名函数传递给另外一个函数

a()->
  Secret = "pony",
  fun() -> Secret end.

b(F) ->
  "a/0's password is " ++ F().

% ============ 重新定义变量

% 这个运行肯定失败，因为变量 A = 1，不能再赋值
base2() ->
  A = 1,
  (fun() -> A = 2 end)().

% 这种写法是合法的，编译时会有 warning 提示定义了一个与父作用域某个变量同名的新名字
% 出现这个提示，可以考虑对变量进行重命名
base3() ->
  A = 1,
  (fun(A) -> A = 2 end)(2).


% ============ 过滤器

% 找出列表元素的所有偶数
even(L) -> lists:reverse(even(L, [])).

even([], Acc) -> Acc;
even([H | T], Acc) when H rem 2 =:= 0 ->
  even(T, [H | Acc]);
even([_| T] , Acc) ->
  even(T, Acc).

% 只保留年龄大于60的男性
old_man(L) -> lists:reverse(old_man(L, [])).

old_man([], Acc) -> Acc;
old_man([{male, Age} | Others], Acc) when Age > 60 ->
  old_man(Others, [{male, Age} | Acc]);
old_man([_ | Others], Acc) ->
  old_man(Others, Acc).

% 对以上两个函数进行抽象

filter(Fun, L) -> lists:reverse(filter(Fun, L, [])).

filter(_, [], Acc) -> Acc;
filter(Fun, [H | T], Acc) ->
  case Fun(H) of
    true -> filter(Fun, T, [H | Acc]);
    false -> filter(Fun, T, Acc)
  end.

% ============ 折叠

% 找出列表中的最大值
max([H|T]) -> max2(T, H).

max2([], Max) -> Max;
max2([H | T], Max) when H > Max -> max2(T, H);
max2([_ | T], Max) -> max2(T, Max).

% 找出列表中的最小值
min([H | T]) -> min2(T, H).

min2([], Max) -> Max;
min2([H | T], Max) when H < Max -> min2(T, H);
min2([_ | T], Max) -> min2(T, Max).

% 计算列表所有元素的和

sum(L) -> sum2(L, 0).
sum2([], Sum) -> Sum;
sum2([H | T], Sum) -> sum2(T, H + Sum).

% 根据上面三个函数构建一个抽象

fold(_, Start, []) -> Start;
fold(Fun, Start, [H | T]) -> fold(Fun, Fun(H, Start), T).










