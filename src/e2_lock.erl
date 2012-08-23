-module(e2_lock).

-behavior(e2_service).

-export([start_link/0,
         start_link/1,
         acquire/2,
         release/2,
         force_release/2]).

-export([debug_state/1]).

-export([handle_msg/3]).

-record(state, {locks, waiters, monitors}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    e2_service:start_link(?MODULE, new_state()).

start_link(Name) when is_atom(Name) ->
    e2_service:start_link(?MODULE, new_state(), [{registered, Name}]).

new_state() ->
    #state{locks=dict:new(),
           waiters=dict:new(),
           monitors=dict:new()}.

acquire(Service, Name) ->
    e2_service:call(Service, {acquire, Name}).

release(Service, Lock) ->
    e2_service:call(Service, {release, Lock}).

force_release(Service, Name) ->
    e2_service:call(Service, {force_release, Name}).

debug_state(Service) ->
    e2_service:call(Service, debug_state).

%%%===================================================================
%%% Message dispatch
%%%===================================================================

handle_msg({acquire, Name}, From, State) ->
    handle_acquire(Name, From, State);
handle_msg({release, Lock}, _From, State) ->
    handle_release(Lock, State);
handle_msg({force_release, Name}, _From, State) ->
    handle_force_release(Name, State);
handle_msg({'$lock_released', Name}, _From, State) ->
    handle_lock_released(Name, State);
handle_msg({'DOWN', Ref, process, _Pid, _Info}, _From, State) ->
    handle_owner_down(Ref, State);
handle_msg(debug_state, _From, State) ->
    {reply, to_proplist(State), State}.

%%%===================================================================
%%% Acquire lock
%%%===================================================================

handle_acquire(Name, From, State) ->
    handle_lock_acquire(lock_available(Name, State), Name, From, State).

lock_available(Name, #state{locks=Locks}) ->
    not dict:is_key(Name, Locks).

handle_lock_acquire(Available, Name, From, State) when Available ->
    Lock = new_lock(Name),
    {reply, {ok, Lock}, monitor_owner(From, Lock, add_lock(Lock, State))};
handle_lock_acquire(_Unavailable, Name, From, State) ->
    {noreply, add_waiter(Name, From, State)}.

new_lock(Name) ->
    {'$lock', Name, erlang:make_ref()}.

add_lock(Lock, #state{locks=Locks}=State) ->
    State#state{locks=dict:store(lock_name(Lock), Lock, Locks)}.

lock_name({'$lock', Name, _Ref}) -> Name.

monitor_owner({Pid, _}, Lock, State) ->
    Ref = erlang:monitor(process, Pid),
    add_monitor(Ref, lock_name(Lock), State).

add_monitor(Ref, Name, #state{monitors=Monitors}=State) ->
    State#state{monitors=dict:store(Ref, Name, Monitors)}.

add_waiter(Name, From, #state{waiters=Waiters}=State) ->
    State#state{waiters=add_waiter(Name, From, Waiters)};
add_waiter(Name, From, Waiters) ->
    WaiterQueue = waiter_queue(Name, Waiters),
    set_waiter_queue(Name, add_waiter(From, WaiterQueue), Waiters).

waiter_queue(Name, Waiters) ->
    new_queue_on_error(dict:find(Name, Waiters)).

new_queue_on_error(error) -> queue:new();
new_queue_on_error({ok, Queue}) -> Queue.

add_waiter(From, Queue) ->
    queue:in(From, Queue).

set_waiter_queue(Name, WaiterQueue, Waiters) ->
    set_or_delete_waiter_queue(
      Name, queue:is_empty(WaiterQueue), WaiterQueue, Waiters).

set_or_delete_waiter_queue(Name, true, _EmptyQueue, Waiters) ->
    dict:erase(Name, Waiters);
set_or_delete_waiter_queue(Name, false, WaiterQueue, Waiters) ->
    dict:store(Name, WaiterQueue, Waiters).

%%%===================================================================
%%% Release lock
%%%===================================================================

handle_release(Lock, State) ->
    handle_release_lock(release_valid_lock(Lock, State), Lock).

handle_force_release(Name, State) ->
    handle_release_lock(release_lock(Name, State), Name).

release_valid_lock(Lock, State) ->
    delete_lock(validate_lock(Lock, State), State).

release_lock(Name, State) ->
    delete_lock(find_lock(Name, State), State).

validate_lock(Lock, State) ->
    compare_lock(find_valid_lock(Lock, State), Lock).

find_valid_lock({'$lock', _, _}=Lock, #state{locks=Locks}) ->
    dict:find(lock_name(Lock), Locks);
find_valid_lock(_, _) -> error.

find_lock(Name, #state{locks=Locks}) ->
    dict:find(Name, Locks).

compare_lock({ok, Lock}, Lock) -> {ok, Lock};
compare_lock(_, _Lock) -> error.

delete_lock({ok, Lock}, #state{locks=Locks}=State) ->
    {ok, State#state{locks=dict:erase(lock_name(Lock), Locks)}};
delete_lock(error, State) ->
    {bad_lock, State}.

handle_release_lock({ok, State}, {'$lock', _, _}=Lock) ->
    handle_release_lock({ok, State}, lock_name(Lock));
handle_release_lock({ok, State}, Name) ->
    notify_lock_released(Name),
    {reply, ok, remove_monitor(Name, State)};
handle_release_lock({bad_lock, State}, _Lock) ->
    {reply, {error, bad_lock}, State}.

notify_lock_released(Name) ->
    erlang:send(self(), {'$lock_released', Name}).

remove_monitor(Name, #state{monitors=Monitors}=State) ->
    State#state{monitors=remove_monitor(Name, Monitors)};
remove_monitor(Name, Monitors) ->
    dict:from_list(remove_monitored_lock(Name, dict:to_list(Monitors))).

remove_monitored_lock(Name, Monitors) ->
    lists:filter(not_lock_filter(Name), Monitors).

not_lock_filter(Target) ->
    fun({_Ref, Name}) -> Name /= Target end.

%%%===================================================================
%%% Lock released
%%%===================================================================

handle_lock_released(Name, State) ->
    handle_lock_grant(grant_lock_to_next_waiter(Name, State)).

handle_lock_grant(State) ->
    {noreply, State}.

grant_lock_to_next_waiter(Name, State) ->
    handle_next_waiter(take_next_waiter(Name, State), Name).

take_next_waiter(Name, #state{waiters=Waiters}=State) ->
    WaiterQueue = waiter_queue(Name, Waiters),
    handle_remove_waiter(remove_waiter(WaiterQueue), Name, State).

remove_waiter(Queue) ->
    queue:out(Queue).

handle_remove_waiter({Waiter, Queue}, Name, #state{waiters=Waiters}=State) ->
    {Waiter, State#state{waiters=set_waiter_queue(Name, Queue, Waiters)}}.

handle_next_waiter({{value, Waiter}, State}, LockName) ->
    Lock = new_lock(LockName),
    e2_service:reply(Waiter, {ok, Lock}),
    monitor_owner(Waiter, Lock, add_lock(Lock, State));
handle_next_waiter({empty, State}, _LockName) ->
    State.

%%%===================================================================
%%% Lock owner DOWN
%%%===================================================================

handle_owner_down(Ref, State) ->
    handle_down_monitor(monitored_lock(Ref, State), State).

monitored_lock(Ref, #state{monitors=Monitors}) ->
    dict:find(Ref, Monitors).

handle_down_monitor({ok, Name}, State) ->
    noreply(handle_force_release(Name, State));
handle_down_monitor(error, State) ->
    {noreply, State}.

noreply({reply, _, State}) ->
    {noreply, State}.

%%%===================================================================
%%% Misc
%%%===================================================================

to_proplist(#state{locks=Locks, waiters=Waiters, monitors=Monitors}) ->
    [{locks, dict:to_list(Locks)},
     {waiters, [{Name, queue:to_list(Queue)}
                || {Name, Queue} <- dict:to_list(Waiters)]},
     {monitors, dict:to_list(Monitors)}].
