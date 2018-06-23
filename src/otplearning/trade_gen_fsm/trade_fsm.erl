%%%-------------------------------------------------------------------
%%% @author gelomenchen
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 六月 2018 下午5:02
%%%-------------------------------------------------------------------
-module(trade_fsm).
-author("gelomenchen").
-behavior(gen_fsm).
-record(state, {name="", other, ownitems=[], otheritems=[], monitor, from}).

%% 公共 API
-export([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2, retract_offer/2, ready/1, cancel/1]).

%% gen_fsm 回调函数
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
		%% 定制的状态名
		idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2, negotiate/3, wait/2, ready/2, ready/3]).


%% ============================================================================
%%                              外部 API
%% ============================================================================



%% ---------------------- 公共 API -------------------------

start(Name) ->
	gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
	gen_fsm:start_link(?MODULE, [Name], []).

trade(OwnPid, OtherPid) ->
	gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

accept_trade(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid, accept_negotiate).

make_offer(OwnPid, Item) ->
	gen_fsm:send_event(OwnPid, {make_offer, Item}).

retract_offer(OwnPid, Item) ->
	gen_fsm:send_event(OwnPid, {retract_offer, Item}).

ready(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid, ready, infinity).

cancel(OwnPid) ->
	gen_fsm:sync_send_all_state_event(OwnPid, cancel).


%% --------------------- FSM 到 FSM -------------------------

ask_negotiate(OtherPid, OwnPid) ->
	gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) ->
	gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

do_offer(OtherPid, Item) ->
	gen_fsm:send_event(OtherPid, {do_offer, Item}).

undo_offer(OtherPid, Item) ->
	gen_fsm:send_event(OtherPid, {undo_offer, Item}).

are_you_ready(OtherPid) ->
	gen_fsm:send_event(OtherPid, are_you_ready).

not_yet(OtherPid) ->
	gen_fsm:send_event(OtherPid, not_yet).

am_ready(OtherPid) ->
	gen_fsm:send_event(OtherPid, 'ready!').

ack_trans(OtherPid) ->
	gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->
	gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
	gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
	gen_fsm:send_all_state_event(OtherPid, cancel).


%% ----------------------- 回调函数 ------------------------

%% ------ 初始状态为 idle，并用玩家名字给 FSM 命名
init(Name) ->
	{ok, idle, #state{name = Name}}.

%% ------ 发送给自己的 FSM

idle({negotiate, OtherPid}, From, S = #state{}) ->
	ask_negotiate(OtherPid, self()),
	notice(S, "asking user ~p for a trade", [OtherPid]),
	Ref = monitor(process, OtherPid),
	{next_state, idle_wait, S#state{other = OtherPid, monitor = Ref, from = From}};
idle(Event, _From, Data) ->
	unexpected(Event, idle),
	{next_state, idle, Data}.

%% ------ FSM 收到的请求

idle({ask_negotiate, OtherPid}, S = #state{}) ->
	Ref = monitor(process, OtherPid),
	notice(S, "~p asked for a trade negotiation", [OtherPid]),
	{next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
	unexpected(Event, idle),
	{next_state, idle, Data}.

%% ------ 给对方 FSM 发送交易请求时还可能收到对方 FSM 发过
%%        来的 ask_negotiate 和 accept_negotiate 请求

idle_wait({ask_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
	gen_fsm:reply(S#state.from, ok),
	notice(S, "staring negotiation", []),
	{next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
	gen_fsm:reply(S#state.from, ok),
	notice(S, "staring negotiation", []),
	{next_state, negotiate, S};
idle_wait(Event, Data) ->
	unexpected(Event, idle_wait),
	{next_state, idle_wait, Data}.

%% ------ 自己 FSM 收到对方 ask_negotiate 消息后会转为 idle_wait 状态
%%        所以是在 idle_wait 状态下接收对方的交易请求

idle_wait(accept_negotiate, _From, S = #state{other = OtherPid}) ->
	accept_negotiate(OtherPid, self()),
	notice(S, "accepting negotiation", []),
	{reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
	unexpected(Event, idle_wait),
	{next_state, idle_wait, Data}.

%% ------ 处于交易状态时，自己可以添加和移除物品，并发送信息给对方 FSM
negotiate({make_offer, Item}, S = #state{ownitems = OwnItems}) ->
	do_offer(S#state.other, Item),
	notice(S, "offering ~p", [Item]),
	{next_state, nogetiate, S#state{ownitems = add(Item, OwnItems)}};
negotiate({retract_offer, Item}, S = #state{ownitems = OwnItems}) ->
	undo_offer(S#state.other, Item),
	notice(S, "cancelling offer on ~p", [Item]),
	{next_state, negotiate, S#state{ownitems = remove(Item, OwnItems)}};
%% ------ 处于交易状态时，自己的 FSM 接收对方添加和移除物品消息
negotiate({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
	notice(S, "Other player offering ~p", [Item]),
	{next_state, regotiate, S#state{otheritems = add(Item, OtherItems)}};
negotiate({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
	notice(S, "Other player cancelling offer on ~p", [Item]),
	{next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}};
%% ------ 处于交易状态时，会接收对方发来的 are_you_ready 消息
%%        并且只要不是处于 wait 状态，都是回复 not_yet
negotiate(are_you_ready, S = #state{other = OtherPid}) ->
	io:format("Other user ready to trade.~n"),
	notice(S, "Other user ready to transfer goods: ~n"
			"You get ~p, The other side gets ~p",
			[S#state.otheritems, S#state.ownitems]),
	not_yet(OtherPid),
	{next_state, negotiate, S};
negotiate(Event, Data) ->
	unexpected(Event, negotiate),
	{next_state, negotiate, Data}.

%% ------ 自己的 FSM 收到自己发出的 ready 消息，然后向对方 FSM 发送 are_you_ready 消息
negotiate(ready, From, S = #state{other = OtherPid}) ->
	are_you_ready(OtherPid),
	notice(S, "asking if ready, waiting", []),
	{next_state, wait, S#state{from = From}};
negotiate(Event, _From, Data) ->
	unexpected(Event, negotiate),
	{next_state, negotiate, Data}.

%% ------ 准备好后处于 wait 状态时，若对方进行物品改动，我们这边会立刻
%%        取消准备状态并回到交易状态 negotiate
wait({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
	gen_fsm:reply(S#state.from, offer_changed),
	notice(S, "Other side offering ~p", [Item]),
	{next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};
wait({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
	gen_fsm:reply(S#state.from, offer_changed),
	notice(S, "Ohter side cancelling offer of ~p", [Item]),
	{next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}};
%% ------ 准备好后处于 wait 状态时，收到对方发送的 are_you_ready 消息
%%        因为自己也是准备好的状态，所以直接回复 am_ready，但依然保持 wait 状态
wait(are_you_ready, S = #state{}) ->
	am_ready(S#state.other),
	notice(S, "asked if ready, and I am. Waiting for same reply", []),
	{next_state, wait, S};
%% ------ 对方回应 not_yet 时，继续保持 wait 状态
wait(not_yet, S = #state{}) ->
	notice(S, "Other not ready yet", []),
	{next_state, wait, S};
%% ------ 若对方准备好了会收到对方 'ready!' 的消息，同时我们也会发送相同的消息给对方
wait('ready!', S = #state{}) ->
	am_ready(S#state.other),
	ack_trans(S#state.other),
	gen_fsm:reply(S#state.from, ok),
	notice(S, "Other side is ready. Moving to ready state", []),
	{next_state, ready, S};
wait(Event, Data) ->
	unexpected(Event, wait),
	{next_state, wait, Data}.

%% ------ 因为 wait 状态收到 'ready!' 消息时，会给对方也发送这个消息
%%        然后双方都会发送 ack 消息，在 ready 状态收到 ack 消息后，会
%%        调用 priority/2 函数进行双方 Pid 大小比较，大的会进行 commit
%%        操作，小的保持原来的 ready 状态
ready(ack, S = #state{}) ->
	case priority(self(), S#state.other) of
		true ->
			try
			    notice(S, "asking for commit", []),
			    ready_commit = ask_commit(S#state.other),
			    notice(S, "Ordering commit", []),
			    ok = do_commit(S#state.other),
			    notice(S, "committing...", []),
			    commit(S),
			    {stop, normal, S}
			catch
			    Class: Reason ->
				    notice(S, "commit failed", []),
				    {stop, {Class, Reason}, S}
			end;
		false ->
			{next_state, ready, S}
	end;
ready(Event, Data) ->
	unexpected(Event, ready),
	{next_state, ready, Data}.

%% ------ 因为上面的 ask_commit 和 do_commit 都是同步的，所以要使用 3 参
ready(ask_commit, _From, S) ->
	notice(S, "commiting...", []),
	{reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
	notice(S, "replying to ask_commit", []),
	commit(S),
	{stop, normal, ok, S};
ready(Event, _From, Data) ->
	unexpected(Event, ready),
	{next_state, ready, Data}.


%% ------ 任何一方取消交易会发送一个全局事件，而 handle_event 就是用来接受这个全局事件的
handle_sync_event(cancel, _From, _StateName, S = #state{}) ->
	notify_cancel(S#state.other),
	notice(S, "cancelling trade, sending cancel event", []),
	{stop, cancelled, ok, S};
handle_sync_event(Event, _From, StateName, Data) ->
	unexpected(Event, StateName),
	{next_state, StateName, Data}.

handle_event(cancel, _StateName, S = #state{}) ->
	notice(S, "received cancel event", []),
	{stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
	unexpected(Event, StateName),
	{next_state, StateName, Data}.

%% ------ 处理对方 FSM 崩溃，因为在 idle 状态函数李设置了一个监控器
%%        可以匹配崩溃时的消息
handle_info({'DOWN', Ref, process, Pid, Reason}, _, S = #state{other = Pid, monitor = Ref}) ->
	notice(S, "Other side dead", []),
	{stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
	unexpected(Info, StateName),
	{next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
	{ok, StateName, Data}.

%% ------ 交易结束时时 ready 状态
terminate(normal, ready, S = #state{}) ->
	notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
	ok.


%% ============================================================================
%%                              内部 API
%% ============================================================================

notice(#state{name = N}, Str, Args) ->
	io:format("~s: " ++ Str ++ "~n", [N | Args]).

unexpected(Msg, State) ->
	io:format("~p received unknow event ~p whild in state ~p~n", [self(), Msg, State]).

add(Item, Items) ->
	[Item | Items].

remove(Item, Items) ->
	Items -- [Item].

priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

commit(S = #state{}) ->
	io:format("Transcation completed for ~s. "
			  "Items sent are:~n~p,~n received are:~n~p.~n"
			  "This operation should have some atomic save in a database.~n",
			  [S#state.name, S#state.ownitems, S#state.otheritems]).