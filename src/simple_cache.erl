-module(simple_cache).

%% API
-export([info/0,
         set/2,
         sync_set/2,
         set/3,
         sync_set/3,
         lookup/1,
         lookup/2,
         flush/1,
         flush/0,

         start/0,
         stop/0
        ]).

-type expire() :: infinity | pos_integer().

-export_type([expire/0]).

%%%=============================================================================
%%% API
%%%=============================================================================
-spec info() -> list().
info() ->
    simple_cache_server:info().

-spec set(any(), any()) -> 'ok'.
set(Key, Value) ->
    simple_cache_server:set(Key, Value).

-spec sync_set(any(), any()) -> any().
sync_set(Key, Value) ->
    simple_cache_server:sync_set(Key, Value).

-spec set(any(), any(), expire()) -> 'ok' | {'error','invalid_expire',_}.
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

-spec lookup(any()) -> {'error','not_found'} | {'ok',_}.
lookup(Key) ->
    simple_cache_server:lookup(Key).

-spec lookup(any(), any()) -> {'ok',_}.
lookup(Key, Default) ->
    simple_cache_server:lookup(Key, Default).

-spec flush(any()) -> 'ok'.
flush(Key) ->
    simple_cache_server:flush(Key).

-spec flush() -> 'ok'.
flush() ->
    simple_cache_server:flush().

-spec start() -> 'ok' | {'error',_}.
start() ->
    application:start(simple_cache).

-spec stop() -> 'ok' | {'error',_}.
stop() ->
    application:stop(simple_cache).
