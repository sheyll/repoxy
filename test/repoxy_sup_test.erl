%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_sup_test).

-include_lib("eunit/include/eunit.hrl").

correct_children_test() ->
    process_flag(trap_exit, true),
    M = em:new(),

    em:strict(M, repoxy_facade, start_link, [],
              {return, {ok, self()}}),
    em:strict(M, repoxy_tcp, start_link, [],
              {return, {ok, self()}}),

    em:replay(M),
    Result = repoxy_sup:start_link(),
    em:verify(M),
    ?assertMatch({ok, _}, Result),
    {ok, Pid} = Result,
    exit(Pid, kill).
