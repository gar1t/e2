-module(e2_supervisor_tests).

-include_lib("eunit/include/eunit.hrl").

empty_test() ->
    ?assertEqual(
       {{one_for_one, 1, 1}, []},
       e2_supervisor:supervisor_spec([], [])).

empty_child_test() ->
    ?assertEqual(
       {{one_for_one, 1, 1},
        [{foo, {foo, start_link, []},
          permanent, brutal_kill, worker, [foo]}]},
       e2_supervisor:supervisor_spec([foo], [])).

sup_type_test() ->
    ?assertMatch(
       {{one_for_all, _, _}, _},
       e2_supervisor:supervisor_spec([], [one_for_all])),
    ?assertMatch(
       {{rest_for_one, _, _}, _},
       e2_supervisor:supervisor_spec([], [rest_for_one])),
    ?assertMatch(
       {{one_for_one, _, _}, _},
       e2_supervisor:supervisor_spec([], [one_for_one])),
    ?assertMatch(
       {{simple_one_for_one, _, _}, _},
       e2_supervisor:supervisor_spec([], [simple_one_for_one])),
    ?assertError(
       {badarg, bad_val},
       e2_supervisor:supervisor_spec([], [bad_val])).

max_restart_test() ->
    ?assertMatch(
       {{one_for_one, 5, 5}, _},
       e2_supervisor:supervisor_spec([], [{max_restart, {5, 5}}])),
    ?assertError(
       {badarg, max_restart},
       e2_supervisor:supervisor_spec([], [{max_restart, so_bad}])),
    ?assertError(
       {badarg, max_restart},
       e2_supervisor:supervisor_spec([], [{max_restart, {-1, 0}}])).

child_id_test() ->
    ?assertMatch(
       {_, [{foo, _, _, _, _, _}]},
       e2_supervisor:supervisor_spec([foo], [])),
    ?assertMatch(
       {_, [{foo_service, _, _, _, _, _}]},
       e2_supervisor:supervisor_spec([{foo, [{id, foo_service}]}], [])).

child_mfa_test() ->
    ?assertMatch(
       {_, [{_, {foo, start, []}, _, _, _, _}]},
       e2_supervisor:supervisor_spec([{foo, start, []}], [])),
    ?assertMatch(
       {_, [{foo2, {foo, start, []}, _, _, _, _}]},
       e2_supervisor:supervisor_spec([{{foo, start, []}, [{id, foo2}]}], [])),
    ?assertMatch(
       {_, [{_, {foo, start_link, [1, 2]}, _, _, _, _}]},
       e2_supervisor:supervisor_spec([{foo, start_link, [1, 2]}], [])).

child_restart_test() ->
    ?assertMatch(
       {_, [{_, _, permanent, _, _, _}]},
       e2_supervisor:supervisor_spec([{foo, [permanent]}], [])),
    ?assertMatch(
       {_, [{_, _, temporary, _, _, _}]},
       e2_supervisor:supervisor_spec([{foo, [temporary]}], [])),
    ?assertMatch(
       {_, [{_, _, transient, _, _, _}]},
       e2_supervisor:supervisor_spec([{foo, [transient]}], [])),
    ?assertMatch(
       {_, [{_, _, permanent, _, _, _}]},
       e2_supervisor:supervisor_spec([{foo, [{restart, permanent}]}], [])).

child_shutdown_test() ->
    ?assertMatch(
       {_, [{_, _, _, brutal_kill, _, _}]},
       e2_supervisor:supervisor_spec([{foo, [{shutdown, brutal_kill}]}], [])),
    ?assertMatch(
       {_, [{_, _, _, 1000, _, _}]},
       e2_supervisor:supervisor_spec([{foo, [{shutdown, 1000}]}], [])),
    ?assertError(
       {badarg, shutdown},
       e2_supervisor:supervisor_spec([{foo, [{shutdown, -1}]}], [])).

child_type_test() ->
    ?assertMatch(
       {_, [{_, _, _, _, worker, _}]},
       e2_supervisor:supervisor_spec([{foo, [worker]}], [])),
    ?assertMatch(
       {_, [{_, _, _, _, supervisor, _}]},
       e2_supervisor:supervisor_spec([{foo, [supervisor]}], [])).

bad_child_option_test() ->
    ?assertError(
       {badarg, worker2},
       e2_supervisor:supervisor_spec([{foo, [worker2]}], [])),
    ?assertError(
       {badarg, something_weird},
       e2_supervisor:supervisor_spec([{foo, [{something_weird, 1}]}], [])).
