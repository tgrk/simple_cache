-module(simple_cache_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CACHE_NAME, test).

%% =============================================================================
simple_cache_test_() ->
    {setup,
         fun setup/0,
         fun teardown/1,
        [
         {"Set/lookup test",         fun test_set_lookup/0},
         {"Lookup default test",     fun test_lookup_default/0},
         {"Set zero expire",         fun test_zero_expire/0},
         {"Set invalid expire",      fun test_invalid_expire/0},
         {"Expire test",             fun test_expire/0},
         {"Flush entry test",        fun test_flush/0},
         {"Flush all test",          fun test_flush_all/0},
         {"Sync flush all test",     fun test_sync_flush_all/0},
         {"Info operations test",    fun test_ops_info/0},
         {"List operations test",    fun test_ops_list/0},
         {"Sync set/lookup test",    fun test_sync_set_lookup/0},
         {"Sync set zero expire",    fun test_sync_zero_expire/0},
         {"Sync expire test",        fun test_sync_expire/0},
         {"Sync set invalid expire", fun test_sync_invalid_expire/0},
         {"Conditional set",         fun test_conditional_set/0},
         {"Multiple caches",         fun test_multiple_caches/0}
        ]
    }.

%% =============================================================================

setup() ->
    application:start(simple_cache),
    ok = simple_cache:new(?CACHE_NAME).

teardown(_) ->
    ok = simple_cache:delete(?CACHE_NAME),
    application:stop(simple_cache).

test_set_lookup() ->
    simple_cache:set(?CACHE_NAME, <<"foo">>, <<"bar">>),
    sleep(),
    ?assertEqual(
       {ok, <<"bar">>},
       simple_cache:lookup(?CACHE_NAME, <<"foo">>, 1)
      ).

test_zero_expire() ->
    simple_cache:set(?CACHE_NAME, <<"foo">>, <<"bar">>),
    sleep(),
    ?assertEqual(
       ok,
       simple_cache:set(?CACHE_NAME, <<"foo">>, <<"bar">>, 0)
      ),
    sleep(500),
    ?assertEqual(
       {error, not_found},
       simple_cache:lookup(?CACHE_NAME, <<"foo">>)
      ).

test_invalid_expire() ->
    ?assertEqual(
       {error, invalid_expire,foo},
       simple_cache:set(?CACHE_NAME, <<"foo">>, <<"bar">>, foo)
      ).

test_lookup_default() ->
    flush(),
    ?assertEqual(
       {ok, <<"bar1">>},
       simple_cache:lookup(?CACHE_NAME, <<"foo">>, <<"bar1">>)
      ).

test_flush() ->
    simple_cache:set(?CACHE_NAME, <<"foo1">>, <<"bar1">>),
    simple_cache:set(?CACHE_NAME, <<"foo2">>, <<"bar2">>),
    simple_cache:set(?CACHE_NAME, <<"foo3">>, <<"bar3">>),
    sleep(),
    simple_cache:flush(?CACHE_NAME, <<"foo2">>),
    simple_cache:flush(?CACHE_NAME, <<"foo3">>),
    sleep(),
    ?assertEqual(
       {ok, <<"bar1">>},
       simple_cache:lookup(?CACHE_NAME, <<"foo1">>)
      ).

test_flush_all() ->
    simple_cache:set(?CACHE_NAME, <<"foo1">>, <<"bar1">>),
    simple_cache:set(?CACHE_NAME, <<"foo2">>, <<"bar2">>),
    simple_cache:set(?CACHE_NAME, <<"foo3">>, <<"bar3">>),
    flush(),
    ?assertEqual(
       {error, not_found},
       simple_cache:lookup(?CACHE_NAME, <<"foo1">>)
      ).

test_sync_flush_all() ->
    simple_cache:sync_set(?CACHE_NAME, <<"foo1">>, <<"bar1">>),
    ?assertEqual({ok, <<"bar1">>},
                 simple_cache:lookup(?CACHE_NAME, <<"foo1">>)),
    ok = simple_cache:sync_flush(?CACHE_NAME),
    ?assertEqual({error, not_found},
                 simple_cache:lookup(?CACHE_NAME, <<"foo1">>)).

test_expire() ->
    simple_cache:set(?CACHE_NAME, <<"foo1">>, <<"bar1">>, 1),
    sleep(1500),
    simple_cache:set(?CACHE_NAME, <<"foo2">>, <<"bar2">>),
    sleep(),
    ?assertEqual(
       {error, not_found},
       simple_cache:lookup(?CACHE_NAME, <<"foo1">>)
      ),
    ?assertEqual(
       {ok, <<"bar2">>},
       simple_cache:lookup(?CACHE_NAME, <<"foo2">>)
      ).

test_ops_info() ->
    simple_cache:sync_set(?CACHE_NAME, <<"foo">>, <<"bar">>, 2),
    ?assertEqual(
       1,
       proplists:get_value(size, simple_cache:ops_info(?CACHE_NAME))
      ).

test_ops_list() ->
    simple_cache:sync_set(?CACHE_NAME, <<"foo">>, <<"bar">>, 2),
    ?assertEqual(
       [{<<"foo">>,<<"bar">>, 2}],
       simple_cache:ops_list(?CACHE_NAME)
      ).

test_sync_set_lookup() ->
    simple_cache:sync_set(?CACHE_NAME, <<"foo">>, <<"bar">>),
    ?assertEqual(
       {ok, <<"bar">>},
       simple_cache:lookup(?CACHE_NAME, <<"foo">>, 1)
      ).

test_sync_zero_expire() ->
    simple_cache:sync_set(?CACHE_NAME, <<"foo">>, <<"bar">>),
    ?assertEqual(
       ok,
       simple_cache:sync_set(?CACHE_NAME, <<"foo">>, <<"bar">>, 0)
      ),
    ?assertEqual(
       {error, not_found},
       simple_cache:lookup(?CACHE_NAME, <<"foo">>)
      ).

test_sync_expire() ->
    simple_cache:sync_set(?CACHE_NAME, <<"foo1">>, <<"bar1">>, 1),
    simple_cache:sync_set(?CACHE_NAME, <<"foo2">>, <<"bar2">>),

    sleep(1500),

    ?assertEqual(
       {error, not_found},
       simple_cache:lookup(?CACHE_NAME, <<"foo1">>)
      ),
    ?assertEqual(
       {ok, <<"bar2">>},
       simple_cache:lookup(?CACHE_NAME, <<"foo2">>)
      ).

test_sync_invalid_expire() ->
    ?assertEqual(
       {error, invalid_expire,foo},
       simple_cache:sync_set(?CACHE_NAME, <<"foo">>, <<"bar">>, foo)
      ).

test_conditional_set() ->
    ok = simple_cache:sync_set(?CACHE_NAME, <<"foo">>, <<"bar">>),
    ?assertEqual({ok, <<"bar">>}, simple_cache:lookup(?CACHE_NAME, <<"foo">>)),
    ?assertEqual({ok, false},
                 simple_cache:cond_set(?CACHE_NAME, <<"foo">>, <<"baz">>,
                                       fun (<<"bar">>) -> false end, infinity)),
    ?assertEqual({ok, <<"bar">>}, simple_cache:lookup(?CACHE_NAME, <<"foo">>)),
    ?assertEqual({ok, true},
                 simple_cache:cond_set(?CACHE_NAME, <<"foo">>, <<"baz">>,
                                       fun (<<"bar">>) -> true end, infinity)),
    ?assertEqual({ok, <<"baz">>},
                 simple_cache:lookup(?CACHE_NAME, <<"foo">>)).

test_multiple_caches() ->
    simple_cache:new(first),
    simple_cache:new(second),

    ?assertEqual([?CACHE_NAME, first, second], simple_cache:list()),

    simple_cache:sync_set(first, a, 1),
    simple_cache:sync_set(second, b, 2),

    ?assertEqual({ok, 1}, simple_cache:lookup(first, a)),
    ?assertEqual({ok, 2}, simple_cache:lookup(second, b)),
    ?assertNotEqual({ok, 1}, simple_cache:lookup(second, a)),
    ?assertNotEqual({ok, 2}, simple_cache:lookup(first, b)),

    flush(first, a),
    flush(second, b),

    ?assertEqual({error, not_found}, simple_cache:lookup(first, a)),
    ?assertEqual({error, not_found}, simple_cache:lookup(second, b)),

    simple_cache:delete(first),
    simple_cache:delete(second),

    sleep(),

    ?assertEqual({error, cache_not_found}, simple_cache:lookup(first, a)),
    ?assertEqual({error, cache_not_found}, simple_cache:lookup(second, b)).

%%=============================================================================
%% Internal functionality
%%=============================================================================

flush() ->
    flush(?CACHE_NAME).

flush(Name) ->
    simple_cache:flush(Name),
    sleep(),
    ok.

flush(Name, Key) ->
    simple_cache:flush(Name, Key),
    sleep(),
    ok.

sleep() ->
    timer:sleep(100).

sleep(Ms) ->
    timer:sleep(Ms).
