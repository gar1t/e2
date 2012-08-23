-module(locks).

-export([run/0]).

run() ->
    {ok, Locks} = e2_lock:start_link(),

    %% Acquire and release - trivial test.
    io:format("Empty: ~p~n", [e2_lock:debug_state(Locks)]),

    {ok, L1} = e2_lock:acquire(Locks, l1),
    io:format("One lock, no waiters: ~p~n", [e2_lock:debug_state(Locks)]),

    ok = e2_lock:release(Locks, L1),
    io:format("Empty: ~p~n", [e2_lock:debug_state(Locks)]),

    %% Acquire and release bogus locks.
    {ok, L2} = e2_lock:acquire(Locks, l2),
    {error, bad_lock} = e2_lock:release(Locks, {lock, l2, some_ref()}),
    {error, bad_lock} = e2_lock:release(Locks, "not a lock"),
    ok = e2_lock:release(Locks, L2),

    %% Use force release with just the lock name
    {ok, _L3} = e2_lock:acquire(Locks, l3),
    ok = e2_lock:force_release(Locks, l3),
    io:format("Empty: ~p~n", [e2_lock:debug_state(Locks)]),

    %% Demonstrate not-synced processes vs those that use locks
    io:format("Not synced processes:~n"),
    run_processes(not_synced),
    io:format("Synced processes:~n"),
    run_processes({synced, Locks}),
    io:format("Empty: ~p~n", [e2_lock:debug_state(Locks)]),

    ok.

run_processes(Sync) ->
    P1 = spawn_message_process(Sync, "Hello from process 1", 5, self()),
    P2 = spawn_message_process(Sync, "Hello from process 2", 5, self()),
    P3 = spawn_message_process(Sync, "Hello from process 3", 5, self()),
    P4 = spawn_message_process(Sync, "Hello from process 4", 5, self()),
    wait_for([P1, P2, P3, P4]).

wait_for([]) -> ok;
wait_for([P|Rest]) ->
    receive
        {done, P} -> wait_for(Rest)
    end.

spawn_message_process(not_synced, Msg, Count, Parent) ->
    spawn_link(fun() -> print_message(Msg, Count, Parent) end);
spawn_message_process({synced, Locks}, Msg, Count, Parent) ->
    spawn_link(
      fun() ->
              {ok, L} = e2_lock:acquire(Locks, lock),
              print_message(Msg, Count, Parent),
              ok = e2_lock:release(Locks, L)
      end).

some_ref() -> erlang:make_ref().

print_message(_Msg, 0, Parent) ->
    erlang:send(Parent, {done, self()});
print_message(Msg, N, Parent) when N > 0 ->
    io:format("~s~n", [Msg]),
    print_message(Msg, N - 1, Parent).
