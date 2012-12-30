-ifndef(REPOXY_CORE_INCLUDED).
-define(REPOXY_CORE_INCLUDED, 1).

%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%% Created : 19 Sep 2012 by Sven Heyll <sven.heyll@gmail.com>
%%%-------------------------------------------------------------------

-define(APP_NAME_POS, 2).

%% NOTE: APP_NAME_POS must contain the element index in the record tuple
%% of #app_build_cfg.name

-record(app_build_cfg, {name = no_name,
                        lib_paths = [],
                        cwd = no_cwd,
                        src_dir = no_src_dir,
                        test_dir = no_test_dir,
                        erl_opts = [],
                        erl_eunit_opts = [],
                        edoc_opts = []}).


-record(node_backup, {apps_loaded, code_path, loaded_modules}).

-record(prj_cfg, {rebar_cfg = no_rebar_cfg :: rebar_config:config(),
                  build_dir = undefined    :: string() | undefined,
                  app_build_cfgs = []      :: [#app_build_cfg{}]}).

%% Events dispatched by repoxy_evt

-define(on_app_discovered(AppInfo),
        {on_app_discovered, AppInfo}).

-define(otp_app(Name, Keys), {application, Name, Keys}).
-define(on_app_load(AppName, ApplicationKeys),
        {on_app_load, ?otp_app(AppName, ApplicationKeys)}).
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
