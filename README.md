simple_cache
============

Simple Memory-based Erlang cache service using ETS.

This is a second fork of library originally written by [Andrew Stanton](https://github.com/Refefer).

This particular version has a few features:
* provides async and sync acesss API (check for `sync_` prefix).
* API documentation
* type specs
* unit tests
* ops helpers (check for `ops_` prefix)
* conditional sets (credit to Christian Lundgren)


Usage
============

Include it into `rebar.config`:
```erlang
{simple_cache, "",
  {git, "git@github.com:tgrk/simple_cache.git", {branch, "master"}}}
```

Start OTP application:
```erlang
ok = application:start(simple_cache).
```

Insert/update value (optional expiration in ms):
```erlang
ok = simple_cache:set(<<"foo">>, <<"bar">>),
ok = simple_cache:set(<<"foo">>, <<"bar">>, 5000),
ok = simple_cache:sync_set(<<"foo">>, <<"bar">>),
ok = simple_cache:sync_set(<<"foo">>, <<"bar">>, infinity),
```

Insert/update value based on predicate result:
```erlang
PredFun = fun (<<"bar">>) -> false end,
{ok, false} = simple_cache:cond_set<<"foo">>, <<"baz">>, PredFun, infinity).
```

Get value by key (optional default value):
```erlang
{ok, <<"bar">>} = simple_cache:lookup(<<"foo">>),
{ok, <<"bar">>} = simple_cache:lookup(<<"foo">>, <<"default">>),
```

Remove cached values all or by key:
```erlang
ok = simple_cache:flush().
ok = simple_cache:sync_flush().
ok = simple_cache:flush(<<"foo">>).
```

Operations helpers:
```erlang
simple_cache:ops_info().
simple_cache:ops_list().
```


For more information about usage refer to tests.

Credits
============
* Andrew Stanton - https://github.com/Refefer/simple_cache
* Sergio Veiga - https://github.com/darkua/simle_cache
* Gustav Simonsson - https://github.com/Gustav-Simonsson/simple_cache
* Christian Lundgren - https://github.com/chrisavl/simple_cache
