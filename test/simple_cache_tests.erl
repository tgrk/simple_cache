-module(simple_cache_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
simple_cache_test_() ->
    {setup,
        fun() -> simple_cache:start()  end,
        fun(_) -> simple_cache:stop() end,
        [
         {"Set/lookup test",         fun test_set_lookup/0},
         {"Lookup default test",     fun test_lookup_default/0},
         {"Set zero expire",         fun test_zero_expire/0},
         {"Set invalid expire",      fun test_invalid_expire/0},
         {"Expire test",             fun test_expire/0},
         {"Flush entry test",        fun test_flush/0},
         {"Flush all test",          fun test_flush_all/0},
         {"Size info test",          fun test_size_info/0},
         {"Sync set/lookup test",    fun test_sync_set_lookup/0},
         {"Sync set zero expire",    fun test_sync_zero_expire/0},
         {"Sync expire test",        fun test_sync_expire/0},
         {"Sync set invalid expire", fun test_sync_invalid_expire/0}
        ]
    }.

%% =============================================================================
test_set_lookup() ->
    simple_cache:set(<<"foo">>, <<"bar">>),
    sleep(),
    ?assertEqual(
       {ok, <<"bar">>},
       simple_cache:lookup(<<"foo">>, 1)
      ).

test_zero_expire() ->
    simple_cache:set(<<"foo">>, <<"bar">>),
    sleep(),
    ?assertEqual(
       ok,
       simple_cache:set(<<"foo">>, <<"bar">>, 0)
      ),
    sleep(500),
    ?assertEqual(
       {error, not_found},
       simple_cache:lookup(<<"foo">>)
      ).

test_invalid_expire() ->
    ?assertEqual(
       {error, invalid_expire,foo},
       simple_cache:set(<<"foo">>, <<"bar">>, foo)
      ).

test_lookup_default() ->
    flush(),
    ?assertEqual(
       {ok, <<"bar1">>},
       simple_cache:lookup(<<"foo">>, <<"bar1">>)
      ).

test_flush() ->
    simple_cache:set(<<"foo1">>, <<"bar1">>),
    simple_cache:set(<<"foo2">>, <<"bar2">>),
    simple_cache:set(<<"foo3">>, <<"bar3">>),
    sleep(),
    simple_cache:flush(<<"foo2">>),
    simple_cache:flush(<<"foo3">>),
    sleep(),
    ?assertEqual(
       {ok, <<"bar1">>},
       simple_cache:lookup(<<"foo1">>)
      ).

test_flush_all() ->
    simple_cache:set(<<"foo1">>, <<"bar1">>),
    simple_cache:set(<<"foo2">>, <<"bar2">>),
    simple_cache:set(<<"foo3">>, <<"bar3">>),
    flush(),
    ?assertEqual(
       {error, not_found},
       simple_cache:lookup(<<"foo1">>)
      ).

test_expire() ->
    simple_cache:set(<<"foo1">>, <<"bar1">>, 1),
    sleep(1500),
    simple_cache:set(<<"foo2">>, <<"bar2">>),
    sleep(),
    ?assertEqual(
       {error, not_found},
       simple_cache:lookup(<<"foo1">>)
      ),
    ?assertEqual(
       {ok, <<"bar2">>},
       simple_cache:lookup(<<"foo2">>)
      ).

test_size_info() ->
    simple_cache:sync_set(<<"foo">>, <<"bar">>),
    ?assertEqual(
       1,
       proplists:get_value(size, simple_cache:info())
      ).

test_sync_set_lookup() ->
    simple_cache:sync_set(<<"foo">>, <<"bar">>),
    ?assertEqual(
       {ok, <<"bar">>},
       simple_cache:lookup(<<"foo">>, 1)
      ).

test_sync_zero_expire() ->
    simple_cache:sync_set(<<"foo">>, <<"bar">>),
    ?assertEqual(
       ok,
       simple_cache:sync_set(<<"foo">>, <<"bar">>, 0)
      ),
    ?assertEqual(
       {error, not_found},
       simple_cache:lookup(<<"foo">>)
      ).

test_sync_expire() ->
    simple_cache:sync_set(<<"foo1">>, <<"bar1">>, 1),
    simple_cache:sync_set(<<"foo2">>, <<"bar2">>),

    sleep(1500),

    ?assertEqual(
       {error, not_found},
       simple_cache:lookup(<<"foo1">>)
      ),
    ?assertEqual(
       {ok, <<"bar2">>},
       simple_cache:lookup(<<"foo2">>)
      ).

test_sync_invalid_expire() ->
    ?assertEqual(
       {error, invalid_expire,foo},
       simple_cache:sync_set(<<"foo">>, <<"bar">>, foo)
      ).

%%=============================================================================
%% Internal functionality
%%=============================================================================
flush() ->
    simple_cache:flush(),
    sleep(),
    ok.

sleep() ->
    timer:sleep(100).

sleep(Ms) ->
    timer:sleep(Ms).
