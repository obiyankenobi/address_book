#!/usr/bin/env escript
%% -*- erlang -*-

main([]) ->
    {ok, Conf} = file:consult("reltool.config"),
    {ok, Spec} = reltool:get_target_spec(Conf),
    reltool:eval_target_spec(Spec, code:root_dir(), "rel").
