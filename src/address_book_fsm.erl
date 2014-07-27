-module(address_book_fsm).
-behaviour(gen_fsm).
-include("address_book.hrl").

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

%% API
-export([start_link/6, start_link/5]).

%% States
-export([prepare/2, waiting/2, get/2, put/2]).

%% This module implements the coordinator fsm for all operations
%% on the address book. One fsm will be spawned for each
%% operation on the fly.

-record(state, {req_id,
                from,
                action,
                name,
                value,
                options,
                preflist,
                num_replies=0,
                replies=[]}).

-define(AB_BUCKET, <<"address_book">>).
-define(MASTER, address_book_vnode_master).


%%%===================================================================
%%% API
%%%===================================================================

%start_link(get, From, Name) ->
%    start_link(From, get, Name, value, ?DEFAULT_OPTIONS).
%
%start_link(get, From, Name, Options) ->
%    start_link(From, get, Name, value, Options);

start_link(ReqId, From, Action, Name, Value) ->
    start_link(ReqId, From, Action, Name, Value, ?DEFAULT_OPTIONS).

start_link(ReqId, From, Action, Name, Value, Options) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Action, Name, Value, Options], []).



%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

init([ReqId, From, Action, Name, Value, Options]) ->
    State = #state{req_id=ReqId,
                   from=From,
                   action=Action,
                   name=Name,
                   value=Value,
                   options=Options},
    {ok, prepare, State, 0}.

prepare(timeout, State=#state{action=Action,name=Name,options=Options}) ->
    {N, _RW, Timeout} = Options,
    %% kill this process if it's still running after Timeout seconds
    timer:kill_after(Timeout, timeout),
    DocIdx = riak_core_util:chash_key({?AB_BUCKET, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_apl(DocIdx, N, address_book),
    NewState = State#state{preflist=PrefList},
    {next_state, Action, NewState, 0}.

get(timeout, State=#state{req_id=ReqId,name=Name,preflist=PrefList}) ->
    issue_command(PrefList, {get, ReqId, Name}),
    {next_state, waiting, State}.

put(timeout, State=#state{req_id=ReqId,name=Name,value=Value,preflist=PrefList}) ->
    issue_command(PrefList, {put, ReqId, Name, Value}),
    {next_state, waiting, State}.

waiting({ok, ReqId, Value}, State=#state{req_id=ReqId,from=From,options=Options,num_replies=NumReplies0,replies=Replies0}) ->
    ?LOG({waiting_answer, Value}),
    NumReplies = NumReplies0 + 1,
    Replies = [Value | Replies0],
    {_N, RepliesNeeded, _Timeout} = Options,
    NewState = State#state{num_replies=NumReplies,replies=Replies},
    if
        NumReplies =:= RepliesNeeded ->
            Reply =
                case lists:any(different(Value), Replies) of
                    true ->
                        %% not all replies were equal. Let client decide correct value
                        Replies;
                    false ->
                        %% if they are all the same, send only one value
                        Value
                end,
            %% send result back to client (From)
            From ! {ReqId, ok, Reply},
            {stop, normal, NewState};
        true ->
            %% wait for more vnodes to replies
            {next_state, waiting, NewState}
    end.

handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> 
    {ok, StateName, State}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

different(A) -> 
    fun(B) -> A =/= B end.

issue_command(PrefList, Args) ->
    riak_core_vnode_master:command(PrefList,
                                   Args,
                                   {fsm, undefined, self()},
                                   ?MASTER).

