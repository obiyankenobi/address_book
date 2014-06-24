-module(address_book_vnode).
-behaviour(riak_core_vnode).
-include("address_book.hrl").
-include_lib("/media/Data/development/riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).


%%%===================================================================
%%% API
%%%===================================================================

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

init([_Index]) ->
    {ok, dict:new()}.

handle_command({put, ReqId, Name, Address}, Sender, State) ->
    ?LOG({put, Name, Address, Sender}),
    {reply, ok, dict:store(Name, Address, State)};

handle_command({get, ReqId, Name}, Sender, State) ->
    ?LOG({get, Name, Sender}),
    case dict:find(Name, State) of
        {ok, Address} ->
            Reply = Address;
        error ->
            Reply = not_found
    end,
    ?LOG({result, Reply}),
    {reply, Reply, State};

handle_command(Message, _Sender, State) ->
    ?LOG({unhandled_command, Message}),
    {noreply, State}.

handle_exit(_Pid, Reason, State) ->
    {stop, Reason, State}.

handle_handoff_command(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(FoldFun, Acc0, State),
    ?LOG({handoff_command, sending_data}),
    {reply, Acc, State};

handle_handoff_command(Message, Sender, State) ->
    ?LOG({handoff_command, Sender, Message}),
    {forward, State}.

handoff_starting(TargetNode, State) ->
    ?LOG({handoff_starting, TargetNode}),
    {true, State}.

handoff_cancelled(State) ->
    ?LOG(handoff_cancelled),
    {ok, State}.

handoff_finished(TargetNode, State) ->
    ?LOG({handoff_finished, TargetNode}),
    {ok, State}.

handle_handoff_data(Data, State) ->
    {Name, Address} = binary_to_term(Data),
    {reply, ok, dict:store(Name, Address, State)}.

encode_handoff_item(ObjectName, ObjectValue) ->
    ?LOG({encode_handoff_item,ObjectName,ObjectValue}),
    term_to_binary({ObjectName,ObjectValue}).

is_empty(State) ->
    ?LOG({is_empty,State}),
    case dict:size(State) of
        0 ->
            {true, State};
        Size ->
            {false, Size, State}
    end.

delete(State) ->
    ?LOG({delete,State}),
    {ok, State}.

handle_coverage(Request, KeySpaces, Sender, State) ->
    ?LOG({handle_coverage, Request, KeySpaces, Sender}),
    {stop, not_implemented, State}.

terminate(_Reason, _State) ->
    ok.
