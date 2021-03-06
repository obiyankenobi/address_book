-module(address_book_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%% Supervisor callbacks

init([]) ->
    VMaster = {address_book_vnode_master,
                {riak_core_vnode_master, start_link, [address_book_vnode]},
                permanent, 5000, worker, [riak_core_vnode_master]},
    FSMMaster = {address_book_fsm_sup,
                {address_book_fsm_sup, start_link, []},
                permanent, infinity, worker, [address_book_fsm_sup]},
    {ok, {{one_for_one, 5, 10}, 
          [VMaster, FSMMaster]}}.
