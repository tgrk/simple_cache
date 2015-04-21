-module(simple_cache).

%% API
-export([ops_info/0,
         ops_list/0,
         set/2, set/3,
         sync_set/2, sync_set/3,
         cond_set/4,
         lookup/1, lookup/2,
         flush/0, flush/1,
         sync_flush/0]).

-type expire() :: infinity | non_neg_integer().
-type conditional() :: fun((any()) -> boolean()).

-export_type([expire/0,
              conditional/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec ops_info() -> list().
ops_info() ->
    simple_cache_server:ops_info().

-spec ops_list() -> list().
ops_list() ->
    simple_cache_server:ops_list().

-spec set(any(), any()) -> ok.
set(Key, Value) ->
    simple_cache_server:set(Key, Value).

-spec sync_set(any(), any()) -> any().
sync_set(Key, Value) ->
    simple_cache_server:sync_set(Key, Value).

-spec set(any(), any(), expire()) -> ok | {error, invalid_expire, any()}.
set(Key, _Value, 0) ->
    simple_cache_server:flush(Key);
set(Key, Value, Expires) when is_number(Expires) ->
    simple_cache_server:set(Key, Value, Expires);
set(_Key, _Value, Expires) ->
    {error, invalid_expire, Expires}.

-spec sync_set(any(), any(), expire()) -> any().
sync_set(Key, _Value, 0) ->
    simple_cache_server:sync_flush(Key);
sync_set(Key, Value, Expires) when is_number(Expires) ->
    simple_cache_server:sync_set(Key, Value, Expires);
sync_set(_Key, _Value, Expires) ->
    {error, invalid_expire, Expires}.

-spec cond_set(any(), any(), conditional(), expire()) -> any().
cond_set(Key, Value, Conditional, Expires) when Expires > 0 ->
    simple_cache_server:cond_set(Key, Value, Conditional, Expires).

-spec lookup(any()) -> {error,not_found} | {ok, any()}.
lookup(Key) ->
    simple_cache_server:lookup(Key).

-spec lookup(any(), any()) -> {ok,_}.
lookup(Key, Default) ->
    simple_cache_server:lookup(Key, Default).

-spec flush(any()) -> ok.
flush(Key) ->
    simple_cache_server:flush(Key).

-spec flush() -> ok.
flush() ->
    simple_cache_server:flush().

-spec sync_flush() -> ok.
sync_flush() ->
    simple_cache_server:sync_flush().


