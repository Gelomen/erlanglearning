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

%% API
-export([]).


%% ============================================================================
%%                              外部 API
%% ============================================================================



%% ---------------------- 公共 API -------------------------

start(Name) ->
	gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
	gen_fsm:start_link(?MODULE, [Name], []).

%% 请求开始交易会话，当/如果对方接受时返回
trade(OwnPid, OtherPid) ->
	gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

%% 接受某个玩家的交易请求
accept_trade(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid, accept_negotiate).

%% 从物品表中选择一个物品进行交易
make_offer(OwnPid, Item) ->
	gen_fsm:send_event(OwnPid, {make_offer, Item}).

%% 撤销某个交易物品
retract_offer(OwnPid, Item) ->
	gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% 宣布自己准备就绪，当对方也宣布自己就绪时，交易就完成了
ready(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% 取消交易
cancel(OwnPid) ->
	gen_fsm:sync_send_all_state_event(OwnPid, cancel).





%% --------------------- FSM 到 FSM 的函数 -------------------------

%% 向另外一个 FSM 发起交易会话请求
ask_negotiate(OtherPid, OwnPid) ->
	gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) ->
	gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%% 转发玩家的交易物品提供消息
do_offer(OtherPid, Item) ->
	gen_fsm:send_event(OtherPid, {do_offer, Item}).

%% 转发玩家的交易物品撤销消息
undo_offer(OtherPid, Item) ->
	gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% 询问对方是否就绪
are_you_ready(OtherPid) ->
	gen_fsm:send_event(OtherPid, are_you_ready).

%% 回复未就绪，也就是说，不在 ‘wait’ 状态
not_yet(OtherPid) ->
	gen_fsm:send_event(OtherPid, not_yet).

%% 通知对方玩家当前处于等待进入 ready 状态
%% 状态会迁移到 'ready'
am_ready(OtherPid) ->
	gen_fsm:send_event(OtherPid, 'ready!').

%% 确认 FSM 处于 ready 状态
ack_trans(OtherPid) ->
	gen_fsm:send_event(OtherPid, ack).

%% 询问是否可以提交
ask_commit(OtherPid) ->
	gen_fsm:sync_send_event(OtherPid, ask_commit).

%% 开始同步提交
do_commit(OtherPid) ->
	gen_fsm:sync_send_event(OtherPid, do_commit).

%% 取消交易时通知对方的 FSM
notify_cancel(OtherPid) ->
	gen_fsm:send_all_state_event(OtherPid, cancel).




%% ----------------- gen_fsm 回调函数 ---------------------

%% init/1 函数只需要玩家名字，其实状态是 idle
init(Name) ->
	{ok, idle, #state{name = Name}}.

%% 为了让我们知道一切运行正常，需要几个工具函数
%% 给玩家发送一条通知，可以是一条发给玩家进程的消息
%% 不过在此处，打印到 shell 上就足够了
notice(#state{name = N}, Str, Args) ->
	io:format("~s: " ++ Str ++ "~n", [N | Args]).

%% 记录非期望的消息
unexpected(Msg, State) ->
	io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

%% 实现异步版本的函数
idle({ask_negotiate, OtherPid}, S = #state{}) ->
	Ref = erlang:monitor(process, OtherPid),
	notice(S, "~p asked for a trade negotiation", [OtherPid]),
	{next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
	unexpected(Event, idle),
	{next_state, idle, Data}.

%% 本方玩家请求 FSM 联系另一个玩家进行交易
idle({negotiate, OtherPid}, From, S=#state{}) ->
	ask_negotiate(OtherPid, self()),
	notice(S, "asking user ~p for a trade", [OtherPid]),
	Ref = monitor(process, OtherPid),
	{next_state, idle_wait, S#state{other = OtherPid, monitor = Ref, from = From}};
idle(Event, _From, Data) ->
	unexpected(Event, idle),
	{next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
	gen_fsm:reply(S#state.from, ok),
	notice(S, "starting negotiation", []),
	{next_state, negotiation, S};
%% 对方接受了我们的请求，迁移到 negotiate 状态
idle_wait({accept_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
	gen_fsm:reply(S#state.from, ok),
	notice(S, "starting negotiation", []),
	{next_state, negotiate, S};
idle_wait(Event, Data) ->
	unexpected(Event, idle_wait),
	{next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S = #state{other = OtherPid}) ->
	accept_negotiate(OtherPid, self()),
	notice(S, "accepting nogetiation", []),
	{reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
	unexpected(Event, idle_wait),
	{next_state, idle_wait, Data}.

%% 实现提供和移除物品功能
negotiate({make_offer, Item}, S = #state{ownitems = OwnItems}) ->
	do_offer(S#state.other, Item),
	notice(S, "offering ~p", [Item]),
	{next_state, negotiate, S#state{ownitems = add(Item, OwnItems)}};
negotiate({retract_offer, Item}, S = #state{ownitems = OwnItems}) ->
	undo_offer(S#state.other, Item),
	notice(S, "cancelling offer on ~p", [Item]),
	{next_state, negotiate, S#state{ownitems = remove(Item, OwnItems)}};
negotiate({do_offer, Item}, S = #state{ownitems = OtherItems}) ->
	notice(S, "other player offering ~p", [Item]),
	{next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};
negotiate({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
	notice(S, "Other player cancelling offer on ~p", [Item]),
	{next_state, negotiate, S#state{otheritems = OtherItems}};
negotiate(are_you_ready, S = #state{other = OtherPid}) ->
	io:format("Other user ready to trade. ~n"),
	notice(S,"Other user ready to transfer goods: ~n"
	"You get ~p, The other side gets ~p",
		[S#state.otheritems, S#state.ownitems]).









%% ============================================================================
%%                              内部 API
%% ============================================================================


%% 向物品列表增加一件物品
add(Item, Items) ->
	[Item | Items].

%% 从物品列表移除一件物品
remove(Item, Items) ->
	Items -- [Item].