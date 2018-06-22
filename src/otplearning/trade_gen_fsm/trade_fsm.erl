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























%% ============================================================================
%%                              内部 API
%% ============================================================================