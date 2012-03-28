%% ===================================================================
%% @author Garrett Smith <g@rre.tt>
%% @copyright 2011-2012 Garrett Smith
%%
%% @doc Utility for running e2 tasks that are implemented in other
%% functions.
%%
%% This utility is similar to `proc_lib'.
%%
%% @end
%% ===================================================================

-module(e2_simple_task).

-behavior(e2_task).

-export([start_link/1, handle_task/1]).

%%--------------------------------------------------------------------
%% @doc Starts a linked simple task.
%%
%% A simple task is an e2 task that is run by calling function.
%%
%% If specified as a function, the function must be arity 0.
%%
%% @spec (TaskSpec) -> {ok, Pid} | {error, Reason}
%% TaskSpec = Function | {Module, Function, Args}
%% Function = function()
%% Module = atom()
%% Function = atom()
%% Args = [term()]
%% @end
%%--------------------------------------------------------------------

start_link(TaskSpec) ->
    validate_task_spec(TaskSpec),
    e2_task:start_link(?MODULE, TaskSpec).

%% @private
handle_task(Spec) ->
    try apply_task_spec(Spec) of
        _ -> {stop, normal}
    catch
        T:E ->
            {stop, {T, E, erlang:get_stacktrace()}}
    end.

validate_task_spec(Fun) when is_function(Fun) ->
    validate_fun_arity(erlang:fun_info(Fun, arity));
validate_task_spec({M, F, A}) when is_atom(M), is_atom(F), is_list(A) -> ok;
validate_task_spec(_) -> error(badarg).

validate_fun_arity({arity, 0}) -> ok;
validate_fun_arity({arity, _}) -> error(badarg).

apply_task_spec(Fun) when is_function(Fun) -> Fun();
apply_task_spec({M, F, A}) -> erlang:apply(M, F, A).
