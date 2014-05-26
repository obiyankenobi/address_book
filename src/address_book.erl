-module(address_book).
-include_lib("/media/Data/development/riak_core/include/riak_core_vnode.hrl").
-include("address_book.hrl").

-export([add_contact/2,
         find_contact/1]).


%% all objects go for the same bucket
-define(BUCKET_TMP,<<"address_book">>).

%% Public API

add_contact(Name, Address) ->
    command({add_contact, Name, Address}).

find_contact(Name) ->
    command({find_contact, Name}).


%% Internal

command(Cmd) ->
    Name = element(2,Cmd),
    DocIdx = riak_core_util:chash_key({?BUCKET_TMP, Name}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, address_book),
    ?LOG(PrefList),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Cmd, address_book_vnode_master).
    
    
    
    %%Preflist = riak_core_apl:get_apl(DocIdx,1,address_book),
    %%?LOG({prefilist,Preflist}),
    %%riak_core_vnode_master:command(Preflist, Cmd, address_book_vnode_master).


