-module(address_book_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    case address_book_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register(address_book, [{vnode_module, address_book_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(address_book_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler([address_book_node_event_handler, []]),
            ok = riak_core_node_watcher:service_up(address_book, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
