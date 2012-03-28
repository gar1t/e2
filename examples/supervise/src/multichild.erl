-module(multichild).

-behavior(e2_supervisor).

-export([start_link/1]).

start_link(ChildCount) ->
    e2_supervisor:start_link(?MODULE, children(ChildCount)).

children(Count) ->
    [{{service, start_link, [[]]}, [{id, service_name(N)}]}
     || N <- lists:seq(1, Count)].

service_name(I) ->
    list_to_atom("service_" ++ integer_to_list(I)).
