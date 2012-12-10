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

-include("repoxy.hrl").
-include("repoxy_project_server.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% Report all applications that rebar discovers to repoxy_facade.
%% @end
%%------------------------------------------------------------------------------
post_compile(Cfg, Arg) when is_list(Arg) ->
    %% TODO add reltool.config support here??
    IsApp = string:str(Arg, ".app") =/= 0,
    if IsApp ->
            {NewCfg, _} = rebar_app_utils:app_name(Cfg, Arg),
            {AppName, AppData} =
                rebar_config:get_xconf(NewCfg, {appfile, {app_file, Arg}}),
            AppLibDirs = rebar_config:get_local(Cfg, lib_dirs, []),
            LibPaths = [rebar_utils:ebin_dir()|
                        expand_lib_dirs(AppLibDirs, rebar_utils:get_cwd(), [])],
            gen_fsm:send_event(?SERVER, ?app_found(
                                           #app_info{
                                              name = AppName,
                                              config = AppData,
                                              lib_paths = LibPaths,
                                              cwd = filename:absname(
                                                      rebar_utils:get_cwd()),
                                              erl_opts = rebar_utils:erl_opts(Cfg)
                                             }));

       true ->
            ok
    end;
post_compile(_Cfg, Arg) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
expand_lib_dirs([], _Root, Acc) ->
    Acc;
expand_lib_dirs([Dir | Rest], Root, Acc) ->
    Apps = filelib:wildcard(filename:join([Dir, "*", "ebin"])),
    FqApps = [filename:join([Root, A]) || A <- Apps],
    expand_lib_dirs(Rest, Root, Acc ++ FqApps).
