-ifndef(repoxy_project_server).
-define(repoxy_project_server, 1).

-define(SERVER, repoxy_project_server).

-define(load(Dir), {load, Dir}).
-define(unload, unload).
-define(clean_build, clean_build).

-endif.
