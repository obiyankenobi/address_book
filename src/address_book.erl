-module(address_book).
-include_lib("/media/Data/development/riak_core/include/riak_core_vnode.hrl").
-include("address_book.hrl").

-export([add_contact/2,
         find_contact/1]).


%% all objects go for the same bucket
-define(BUCKET_TMP,<<"address_book">>).
%% timeout for requests
-define(REQ_TIMEOUT,timer:seconds(2)).


%%%===================================================================
%%% API
%%%===================================================================

add_contact(Name, Address) ->
    command({put, Name, Address}).

find_contact(Name) ->
    command({get, Name, address_not_used}).


%%%===================================================================
%%% Internal
%%%===================================================================

command(Cmd) ->
    {ok, ReqId} = address_book_fsm_sup:request(Cmd),
    wait_for_request(ReqId, ?REQ_TIMEOUT).

wait_for_request(ReqId, Timeout) ->
    receive
        {ReqId, ok} -> ok;
        {ReqId, ok, Value} -> {ok, Value}
    after Timeout ->
              {error, timeout}
    end.
