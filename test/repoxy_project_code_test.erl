%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 19 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_project_code_test).
-include_lib("eunit/include/eunit.hrl").
-include("repoxy.hrl").

backup_restore_node_test() ->
    %% load some fake my_module to stay
    M1 = em:new(),
    em:strict(M1, my_module_stay, my_fun, []),
    em:replay(M1),
    my_module_stay:my_fun(),
    em:verify(M1),

    OldPath = code:get_path(),
    OldAppLoaded = application:loaded_applications(),
    OldApssStarted = application:which_applications(),
    BackUp = repoxy_project_code:backup_node(),

    %% load some fake my_module
    M = em:new(),
    em:strict(M, my_module, my_fun, []),
    em:replay(M),
    my_module:my_fun(),

    %% add a b/s path
    code:add_patha("/tmp"),

    %% sart some application
    application:load(parsetools),
    application:start(parsetools),

    repoxy_project_code:restore_node(BackUp),

    ?assertEqual(OldPath, code:get_path()),
    ?assertEqual(OldAppLoaded, application:loaded_applications()),
    ?assertEqual(OldApssStarted, application:which_applications()),
    ?assertEqual(false, lists:keyfind(my_module, 1, code:all_loaded())),

    em:verify(M).

load_app_info_test() ->
   ok. %% TODO load_app_info_test

unload_app_info_test() ->
    ok. %% TODO unload_app_info_test
