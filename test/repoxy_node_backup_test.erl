%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%%-------------------------------------------------------------------
-module(repoxy_node_backup_test).
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
    BackUp = repoxy_node_backup:backup_node(),

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

    repoxy_node_backup:restore_node(BackUp),

    ?assertEqual(OldPath, code:get_path()),
    ?assertEqual(OldAppLoaded, application:loaded_applications()),
    ?assertEqual(OldApssStarted, application:which_applications()),
    ?assertEqual(false, lists:keyfind(my_module, 1, code:all_loaded())),

    em:verify(M).
