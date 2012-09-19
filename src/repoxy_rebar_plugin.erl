%%%-----------------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% @doc
%%% Process repoxy commands from inside rebar via hooking into the rebar command
%%% processing pipeline.
%%% @end
%%% Created : 17 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-----------------------------------------------------------------------------

-module(repoxy_rebar_plugin).

-export([post_compile/2]).

%%------------------------------------------------------------------------------
%% @doc
%% Report all applications that rebar discovers to repoxy_facade.
%% @end
%%------------------------------------------------------------------------------
post_compile(Cfg, Arg) when is_list(Arg) ->
    IsApp = string:str(Arg, ".app") =/= 0,
    if IsApp ->
            {NewCfg, _} = rebar_app_utils:app_name(Cfg, Arg),
            {AppName, AppData} = rebar_config:get_xconf(NewCfg, {appfile, {app_file, Arg}}),
            error_logger:info_msg("App: ~p~n", [AppName]),
            rebar_erlc_compiler:compile(Cfg, Arg),
            error_logger:info_msg("AppData: ~p~n", [AppData]),
            error_logger:info_msg("EbinDir: ~p~n", [rebar_utils:ebin_dir()]),
            error_logger:info_msg("erl_opts: ~p~n", [rebar_utils:erl_opts(Cfg)]),
            AppLibDirs = rebar_config:get_local(Cfg, lib_dirs, []),
            LibPaths = [rebar_utils:ebin_dir()|
                        expand_lib_dirs(AppLibDirs, rebar_utils:get_cwd(), [])],
            error_logger:info_msg("AppLibDirs: ~p~n", [LibPaths]),
            code:add_pathsa(LibPaths),
            error_logger:info_msg("Loading App result: ~p~n", [application:load(AppName)]),
            error_logger:info_msg("code lib_dir: ~p~n", [code:lib_dir(AppName)]);

       true ->
            error_logger:info_msg("Skipping: ~p~n", [Arg])
    end;
post_compile(_Cfg, Arg) ->
    error_logger:info_msg("Skipping: ~p~n", [Arg]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
expand_lib_dirs([], _Root, Acc) ->
    Acc;
expand_lib_dirs([Dir | Rest], Root, Acc) ->
    Apps = filelib:wildcard(filename:join([Dir, "*", "ebin"])),
    FqApps = [filename:join([Root, A]) || A <- Apps],
    expand_lib_dirs(Rest, Root, Acc ++ FqApps).
