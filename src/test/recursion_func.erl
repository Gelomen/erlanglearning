%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 六月 2018 下午2:39
%%%-------------------------------------------------------------------
-module(recursion_func).
-author("gelomenchen").

%% API
-export([fac/1]).

%%  递归函数
fac(0) -> 1;
fac(X) when X > 0 ->
  X * fac(X-1).
