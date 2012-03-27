-module(transient_sup).

-behavior(e2_supervisor).

-export([start_link/0]).

start_link() ->
    e2_supervisor:start_link(
      ?MODULE, [{service, [transient]}], [{max_restart, {2, 1}}]).
