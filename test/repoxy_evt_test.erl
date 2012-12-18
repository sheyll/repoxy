%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%%-------------------------------------------------------------------
-module(repoxy_evt_test).
-include_lib("eunit/include/eunit.hrl").
-include("repoxy.hrl").

%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_test() ->
    M = em:new(),
    em:strict(M, test_mod, on_project_event, [test_arg, xxx]),
    em:replay(M),
    repoxy_evt:start_link(),
    repoxy_evt:add_sup_handler(test_mod, test_arg),
    repoxy_evt:notify(xxx),
    em:await_expectations(M),
    process_flag(trap_exit, true),
    exit(whereis(repoxy_evt), kill).
