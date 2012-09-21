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

-include("repoxy_core.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% Report all applications that rebar discovers to repoxy_facade.
%% @end
%%------------------------------------------------------------------------------
post_compile(Cfg, Arg) when is_list(Arg) ->
    IsApp = string:str(Arg, ".app") =/= 0,
    if IsApp ->
            {NewCfg, _} = rebar_app_utils:app_name(Cfg, Arg),
            {AppName, AppData} =
                rebar_config:get_xconf(NewCfg, {appfile, {app_file, Arg}}),
            error_logger:info_msg("Gathering App: ~p~n", [AppName]),
            error_logger:info_msg("AppData: ~p~n", [AppData]),
            error_logger:info_msg("EbinDir: ~p~n", [rebar_utils:ebin_dir()]),
            error_logger:info_msg("erl_opts: ~p~n", [rebar_utils:erl_opts(Cfg)]),
            AppLibDirs = rebar_config:get_local(Cfg, lib_dirs, []),
            LibPaths = [rebar_utils:ebin_dir()|
                        expand_lib_dirs(AppLibDirs, rebar_utils:get_cwd(), [])],
            error_logger:info_msg("AppLibDirs: ~p~n", [LibPaths]),
            repoxy_facade:app_compiled(#app_info{
                                          name = AppName,
                                          config = AppData,
                                          lib_paths = LibPaths,
                                          erl_opts = rebar_utils:erl_opts(Cfg)
                                         });

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
