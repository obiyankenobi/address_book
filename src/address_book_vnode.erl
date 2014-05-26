-module(address_book_vnode).
-behaviour(riak_core_vnode).
-include("address_book.hrl").

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

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

%% vnode callbacks
init([_Index]) ->
    {ok, []}.

handle_command({add_contact, Name, Address}, Sender, State) ->
    ?LOG({add_contact, Name, Address, Sender}),
    {reply, ok, [{Name, Address} | State]};

handle_command({find_contact, Name}, Sender, State) ->
    ?LOG({find_contact, Name, Sender}),
    case lists:keyfind(Name, 1, State) of
        {Name, Address} ->
            Reply = Address;
        false ->
            Reply = "Not found"
    end,
    ?LOG({result, Reply}),
    {reply, Reply, State};

handle_command(Message, _Sender, State) ->
    ?LOG({unhandled_command, Message}),
    {noreply, State}.

handle_exit(_Pid, Reason, State) ->
    {stop, Reason, State}.

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
    ?LOG({handoff_data, Data}),
    binary_to_term(Data),
    {reply, ok, State}.

encode_handoff_item(ObjectName, ObjectValue) ->
    term_to_binary({ObjectName,ObjectValue}).

is_empty(State) ->
    ?LOG({is_empty,State}),
    case length(State) of
        0 ->
            {true, []};
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
