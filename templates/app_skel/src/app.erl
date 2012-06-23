-module({{appid}}_app).

-behavior(e2_application).

-export([init/0]).

%%%===================================================================
%%% e2_application callbacks
%%%===================================================================

init() ->
    e2_log:info("TODO: configure top-level processes for your app"),
    {ok, []}.
