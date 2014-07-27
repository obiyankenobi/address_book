-module(address_book).
-include("address_book.hrl").

-export([add_contact/2,
         find_contact/1]).


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
    {_N, _RW, Timeout} = ?DEFAULT_OPTIONS,
    wait_for_request(ReqId, Timeout).

wait_for_request(ReqId, Timeout) ->
    receive
        {ReqId, ok, ok} -> ok;
        {ReqId, ok, Value} -> {ok, Value}
    after Timeout ->
              {error, timeout}
    end.
