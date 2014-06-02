-module(address_book_node_event_handler).
-behaviour(gen_event).
-include("address_book.hrl").

%% gen_events callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_event({service_update, Services}, State) ->
    ?LOG(Services),
    {ok, State}.

handle_call(Event, State) ->
    %%?LOG(Event),
    {ok, ok, State}.

handle_info(Info, State) ->
    ?LOG(Info),
    {ok, State}.

terminate(Reason, _State) ->
    ?LOG(Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
