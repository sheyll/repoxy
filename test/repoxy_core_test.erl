%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 19 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------
-module(repoxy_core_test).
-include_lib("eunit/include/eunit.hrl").

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
