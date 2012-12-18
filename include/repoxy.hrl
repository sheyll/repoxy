-ifndef(REPOXY_CORE_INCLUDED).
-define(REPOXY_CORE_INCLUDED, 1).

%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 19 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------

-define(APP_NAME_POS, 2).

%% NOTE: APP_NAME_POS must contain the element index in the record tuple
%% of #app_info.name

-record(app_info, {name = no_name,
                   version = no_version,
                   config = no_config,
                   lib_paths = [],
                   cwd = no_cwd,
                   src_dir = no_src_dir,
                   test_dir = no_test_dir,
                   modules = [],
                   erl_opts = [],
                   erl_eunit_opts = [],
                   eunit_opts = [],
                   edoc_opts = []}).

-record(node_backup, {apps_loaded, code_path, loaded_modules}).

-record(prj_cfg, {rebar_cfg = no_rebar_cfg :: rebar_config:config(),
                  base_dir = ""            :: string(),
                  build_dir = undefined    :: string() | undefined,
                  app_infos = []           :: [#app_info{}]}).

%% Events dispatched by repoxy_project_events

-define(on_app_discovered(AppInfo),
        {on_app_discovered, AppInfo}).

-define(on_app_load(AppName),
        {on_app_load, AppName}).
-define(on_app_load_failed(AppName, Reason),
        {on_app_load_failed, AppName, Reason}).

-define(on_app_unload(AppName),
        {on_app_unload, AppName}).
-define(on_app_unload_failed(AppName, Reason),
        {on_app_unload_failed, AppName, Reason}).

-define(on_project_load(PrjCfg),
        {on_project_load, PrjCfg}).
-define(on_project_load_failed(Dir, Reason),
        {on_project_load_failed, Dir, Reason}).

-define(on_project_unload(PrjCfg), {on_unload, PrjCfg}).

-define(on_internal_error(E), {on_internal_error, E}).

-endif.
