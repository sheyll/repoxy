%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 19 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_core_test).
-include_lib("eunit/include/eunit.hrl").
-include("repoxy_core.hrl").

backup_restore_node_test() ->
    OldPath = code:get_path(),
    OldAppLoaded = application:loaded_applications(),
    OldApssStarted = application:which_applications(),

    BackUp = repoxy_core:backup_node(),

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

    repoxy_core:restore_node(BackUp),

    ?assertEqual(OldPath, code:get_path()),
    ?assertEqual(OldAppLoaded, application:loaded_applications()),
    ?assertEqual(OldApssStarted, application:which_applications()),
    ?assertEqual(false, lists:keyfind(my_module, 1, code:all_loaded())),

    em:verify(M).

app_infos_test() ->
    A0 = #app_info{name = app0, config = appcfg0},
    A1 = #app_info{name = app1, config = appcfg1},
    A2 = #app_info{name = app0, config = appcfg2},

    Cfg0 = repoxy_core:empty_prj_cfg(),
    Cfg1 = repoxy_core:add_app_info(A0, Cfg0),
    Cfg2 = repoxy_core:add_app_info(A1, Cfg1),
    Cfg3 = repoxy_core:add_app_info(A2, Cfg2),

    ?assertEqual([A2, A1], repoxy_core:get_app_infos(Cfg3)).

app_paths_test() ->
    A0 = #app_info{name = app0, cwd = f1},
    A1 = #app_info{name = app1, cwd = f2},

    Cfg0 = repoxy_core:empty_prj_cfg(),
    Cfg1 = repoxy_core:add_app_info(A0, Cfg0),
    Cfg2 = repoxy_core:add_app_info(A1, Cfg1),

    ?assertEqual([{app0, f1},
                  {app1, f2}], repoxy_core:get_app_paths(Cfg2)).
