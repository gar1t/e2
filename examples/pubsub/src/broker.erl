-module(broker).

-behavior(e2_publisher).

-export([start_link/0, start_link/1,
         subscribe/1, subscribe/2,
         subscribe_all/0, subscribe_all/1,
         unsubscribe/1, unsubscribe/2,
         unsubscribe_all/0, unsubscribe_all/1,
         publish/1,
         get_tag/0, set_tag/1]).

-export([init/1, handle_msg/3]).

-define(DEFAULT_TAG, a_broker).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link(?DEFAULT_TAG).

start_link(MsgTag) ->
    e2_publisher:start_link(?MODULE, [MsgTag], [registered]).

subscribe(Pattern) ->
    e2_publisher:subscribe(?MODULE, Pattern).

subscribe(Pattern, Subscriber) ->
    e2_publisher:subscribe(?MODULE, Pattern, Subscriber).

subscribe_all() ->
    e2_publisher:subscribe_all(?MODULE).

subscribe_all(Subscriber) ->
    e2_publisher:subscribe_all(?MODULE, Subscriber).

unsubscribe(Pattern) ->
    e2_publisher:unsubscribe(?MODULE, Pattern).

unsubscribe(Pattern, Subscriber) ->
    e2_publisher:unsubscribe(?MODULE, Pattern, Subscriber).

unsubscribe_all() ->
    e2_publisher:unsubscribe_all(?MODULE).

unsubscribe_all(Subscriber) ->
    e2_publisher:unsubscribe_all(?MODULE, Subscriber).

publish(Msg) ->
    e2_publisher:publish(?MODULE, {get_tag(), Msg}).

get_tag() ->
    e2_publisher:call(?MODULE, tag).

set_tag(Tag) ->
    e2_publisher:call(?MODULE, {tag, Tag}).

%%%===================================================================
%%% Publisher callbacks
%%%===================================================================

init([Tag]) ->
    {ok, Tag}.

handle_msg({tag, Tag}, _From, _OldName) ->
    {reply, ok, Tag};
handle_msg(tag, _From, Tag) ->
    {reply, Tag, Tag}.
