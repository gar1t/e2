-module(temporary_sup).

-behavior(e2_supervisor).

-export([start_link/0]).

start_link() ->
    e2_supervisor:start_link(
      ?MODULE, [{service, [temporary]}], [{max_restart, {1, 1}}]).
