-define(LOG(Text), io:format("DEBUG: ~p:~p - ~p~n ~p~n~n", [?MODULE, ?LINE, ??Text, Text])).

%% default options: {N, RepliesNeeded, Timeout}
%% The second parameter depends wether it's a read or write operation.
-define(DEFAULT_OPTIONS, {3, 2, timer:seconds(2)}).
