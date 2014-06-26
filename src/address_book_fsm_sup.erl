-module(address_book_fsm_sup).
-behaviour(supervisor).
-include("address_book.hrl").

-export([start_link/0, init/1, request/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

request({Action, Name, Value}) ->
    ReqId = make_ref(),
    supervisor:start_child(?MODULE, [ReqId, self(), Action, Name, Value]),
    {ok, ReqId}.


%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

init(_Args) ->
    ChildSpec = {undefined,
                 {address_book_fsm, start_link, []},
                 temporary,
                 5000,
                 worker,
                 [address_book_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [ChildSpec]}}.
