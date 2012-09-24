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
-record(app_info,
        {name = no_name,
         config = no_config,
         lib_paths = no_lib_paths,
         erl_opts = no_erl_opts,
         cwd = ""
        }).


-endif.
