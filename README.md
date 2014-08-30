# address_book

A distributed address book. This is a very silly project to try out [riak_core](https://github.com/basho/riak_core/). The contacts stored are distributed and replicated (by default in 2 nodes) so you have fault-tolerance.


## Todo

- use lagger for logging and remove unnecessary log messages;

- better interfaces (modules APIs);

- create update_contact action;

- give users the possibility of using custom options for RW quorum. This already exists, but there's no way of changing the default   values;

- create development release, with more than you node;


## Quick guide

Start by issuing a `make release`. The project should download all dependencies and create the release in the rel folder. Start the node:

```
obiyankenobi@tatoine:~$ rel/address_book/bin/address_book console -sname node1
...
(node1@tatoine)1>
```

In a separate folder, start another node and create the cluster. Since it's in the same computer, we have to use a different handoff port.
```
obiyankenobi@tatoine:/tmp$ rel/address_book/bin/address_book console -riak_core handoff_port 3333 -sname node2
...
(node2@tatoine)1>
(node2@tatoine)1> riak_core:join('node1@tatoine').
ok
```

Now you can safely store your contacts!
```
(node2@tatoine)2> address_book:add_contact("Dennis Ritchie","1 Unix Way").
ok

(node2@tatoine)3> address_book:find_contact("Dennis Ritchie").
{ok,"1 Unix Way"}

(node2@tatoine)4> address_book:find_contact("Dennis Rodman").
{ok,not_found}
```


## More info

The example above used only two nodes, but you can use as many as you'd like. For more information on RW quorum, fault-tolerance and whatnot, read the [RIAK documentation](http://docs.basho.com/riak/latest/theory/concepts/#Clustering).
