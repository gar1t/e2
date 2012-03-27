-module(e2_service_tests).

-include_lib("eunit/include/eunit.hrl").

simple_service1_test() ->
    %% Super simple service - just serves a 'secret' that we provide on start
    {ok, S} = simple_service1:start_link("my secret"),
    ?assertEqual("my secret", simple_service1:get_secret(S)).

simple_service2_test() ->
    %% Slightly more complex service: converts an init string number and
    %% supports an increment of the number state
    {ok, S} = simple_service2:start_link("4"),
    ?assertEqual(4, simple_service2:get_val(S)),
    simple_service2:incr_val(S, 2),
    ?assertEqual(6, simple_service2:get_val(S)).

simple_service3_test() ->
    %% Fancier service that builds a list of events internally and sends
    %% that back as a message to a target process
    {ok, _S} = simple_service3:start_link([start], self()),
    receive
        {simple_service3_result, Events} ->
            ?assertEqual([start, init, post_init, a_msg, {terminate,normal}],
                         lists:reverse(Events))
    after
        100 -> error(timeout)
    end.
