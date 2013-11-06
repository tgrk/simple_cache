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

%%%=============================================================================
%%% API
%%%=============================================================================
info() ->
    simple_cache_server:info().

set(Key, Value) ->
    simple_cache_server:set(Key, Value).

sync_set(Key, Value) ->
    simple_cache_server:sync_set(Key, Value).

set(Key, _Value, 0) ->
    simple_cache_server:flush(Key);
set(Key, Value, Expires) when is_number(Expires) ->
    simple_cache_server:set(Key, Value, Expires);
set(_Key, _Value, Expires) ->
    {error, invalid_expire, Expires}.

sync_set(Key, _Value, 0) ->
    simple_cache_server:sync_flush(Key);
sync_set(Key, Value, Expires) when is_number(Expires) ->
    simple_cache_server:sync_set(Key, Value, Expires);
sync_set(_Key, _Value, Expires) ->
    {error, invalid_expire, Expires}.

lookup(Key) ->
    simple_cache_server:lookup(Key).

lookup(Key, Default) ->
    simple_cache_server:lookup(Key, Default).

flush(Key) ->
    simple_cache_server:flush(Key).

flush() ->
    simple_cache_server:flush().

start() ->
    application:start(simple_cache).

stop() ->
    application:stop(simple_cache).
