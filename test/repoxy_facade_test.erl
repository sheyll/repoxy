%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 13 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_facade_test).
-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
    (catch exit(whereis(repoxy_facade), kill)),
    process_flag(trap_exit, true),
    M = em:new(),
    em:strict(M, repoxy_core, load_rebar, [],
              {return, config}),
    em:replay(M),
    ?assertMatch({ok, _}, repoxy_facade:start_link()),
    em:verify(M),
    (catch exit(whereis(repoxy_facade), kill)).

dispatch_commands_test() ->
    Response = {ok, [ooo, aaa]},
    (catch exit(whereis(repoxy_facade), kill)),
    process_flag(trap_exit, true),
    M = em:new(),
    em:strict(M, repoxy_core, load_rebar, [],
              {return, config}),
    em:strict(M, repoxy_core, xxx, [config, yyy, zzz],
             {return, Response}),
    em:replay(M),
    ?assertMatch({ok, _}, repoxy_facade:start_link()),
    ?assertEqual(Response,
                 repoxy_facade:handle_request([xxx,yyy,zzz])),
    em:verify(M),
    (catch exit(whereis(repoxy_facade), kill)).
