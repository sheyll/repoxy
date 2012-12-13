%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 19 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_facade_test).
-include_lib("eunit/include/eunit.hrl").
-include("repoxy.hrl").
-include("../src/repoxy_project_server.hrl").

%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_existing_function_test() ->
    M = em:new(),
    em:strict(M, repoxy_project, existing_fun, [arg1, arg2, arg3]),
    em:replay(M),
    repoxy_facade:handle_request([existing_fun, arg1, arg2, arg3]),
    em:verify(M).

handle_existing_function_exception_test() ->
    M = em:new(),
    em:strict(M, repoxy_project, existing_fun, [],
              {function, fun(_) -> throw(xxx) end}),
    em:replay(M),
    repoxy_facade:handle_request([existing_fun]),
    em:verify(M).

handle_non_existing_function_test() ->
    repoxy_facade:handle_request([non_existing_fun, arg1]).
