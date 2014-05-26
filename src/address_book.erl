-module(address_book).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([add_contact/2,
         find_contact/1]).


%% all objects go for the same bucket
-define(BUCKET,<<"address_book">>).

%% Public API

add_contact(Name, Address) ->
    command({add_contact, Name, Address}).

find_contact(Name) ->
    command({find_contact, Name}).


%% Internal

command(Cmd) ->
    Name = element(2,Cmd),
    DocIdx = riak_core_util:chash_key({?BUCKET, Name}),
    Preflist = riak_core_apl:get_apl(DocIdx,1,address_book),
    riak_core_vnode_master:command(Preflist, Cmd, address_book_vnode_master).


