%% Simple Service 1
%%
%% This service is as simple as it gets. The service args is used as the
%% initial state because there's not init/1 function. There's one service
%% function implemented as a call.
%%
-module(simple_service1).

-export([start_link/1, get_secret/1]).

-export([handle_msg/3]).

start_link(Secret) ->
    e2_service:start_link(?MODULE, Secret).

get_secret(Service) ->
    e2_service:call(Service, secret).

handle_msg(secret, _From, Secret) ->
    {reply, Secret, Secret}.
