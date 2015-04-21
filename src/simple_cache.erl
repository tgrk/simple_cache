-module(simple_cache).

-include("simple_cache.hrl").

%% API
-export([new/1,
         delete/1,
         ops_info/1,
         ops_list/1,
         set/3, set/4,
         sync_set/3, sync_set/4,
         cond_set/5,
         lookup/2, lookup/3,
         flush/1, flush/2,
         sync_flush/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec new(cache_name()) -> ok.
new(Name) ->
    ok = simple_cache_sup:start_server(init_name(Name)).

-spec delete(cache_name()) -> ok.
delete(Name) ->
    ok = simple_cache_sup:stop_server(name(Name)).

-spec ops_info(cache_name()) -> list().
ops_info(Name) ->
    call(ops_info, Name).

-spec ops_list(cache_name()) -> list().
ops_list(Name) ->
    call(ops_list, Name).

-spec set(cache_name(), any(), any()) -> ok.
set(Name, Key, Value) ->
    call(set, Name, [Key, Value]).

-spec sync_set(cache_name(), any(), any()) -> any().
sync_set(Name, Key, Value) ->
    call(sync_set, Name, [Key, Value]).

-spec set(cache_name(), any(), any(), expire()) -> ok | {error, invalid_expire, any()}.
set(Name, Key, _Value, 0) ->
    call(flush, Name, [Key]);
set(Name, Key, Value, Expires) when is_number(Expires) ->
    call(set, Name, [Key, Value, Expires]);
set(_Name, _Key, _Value, Expires) ->
    {error, invalid_expire, Expires}.

-spec sync_set(cache_name(), any(), any(), expire()) -> any().
sync_set(Name, Key, _Value, 0) ->
    call(sync_flush, Name, [Key]);
sync_set(Name, Key, Value, Expires) when is_number(Expires) ->
    call(sync_set, Name, [Key, Value, Expires]);
sync_set(_Name, _Key, _Value, Expires) ->
    {error, invalid_expire, Expires}.

-spec cond_set(cache_name(), any(), any(), conditional(), expire()) -> any().
cond_set(Name, Key, Value, Conditional, Expires) when Expires > 0 ->
    call(cond_set, Name, [Key, Value, Conditional, Expires]).

-spec lookup(cache_name(), any()) -> {error,not_found} | {ok, any()}.
lookup(Name, Key) ->
    call(lookup, Name, [Key]).

-spec lookup(cache_name(), any(), any()) -> {ok,_}.
lookup(Name, Key, Default) ->
    call(lookup, Name, [Key, Default]).

-spec flush(cache_name(), any()) -> ok.
flush(Name, Key) ->
    call(flush, Name, [Key]).

-spec flush(cache_name()) -> ok.
flush(Name) ->
    call(flush, Name).

-spec sync_flush(cache_name()) -> ok.
sync_flush(Name) ->
    call(sync_flush, Name).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

init_name(Name) when is_atom(Name) ->
    erlang:list_to_atom(
      "simple_cache_" ++ erlang:atom_to_list(Name)).

name(Name) when is_atom(Name) ->
    try
        erlang:list_to_existing_atom(
          "simple_cache_" ++ erlang:atom_to_list(Name))
    catch
        error:badarg ->
            {error, cache_not_found}
    end.

call(Function, Name) ->
    call(Function, Name, []).

call(Function, Name, Args) ->
    case name(Name) of
        {error, cache_not_found} ->
            {error, cache_not_found};
        ServerName ->
            erlang:apply(simple_cache_server, Function, [ServerName | Args])
    end.
