-module(simple_cache_server).

-behaviour(gen_server).

-define(SERVER,      ?MODULE).
-define(ETS_OPTIONS, [{read_concurrency, true},
                      {write_concurrency, true},
                      named_table,
                      protected
                     ]).

-record(state, {table}).

%%=============================================================================
%% API Function Exports
%%=============================================================================
-export([start_link/0,
         info/0,
         sync_set/2,
         set/2,
         sync_set/3,
         set/3,
         lookup/1,
         lookup/2,
         flush/1,
         sync_flush/1,
         flush/0
        ]).

%%=============================================================================
%% gen_server Function Exports
%%=============================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%=============================================================================
%% API Function Definitions
%%=============================================================================
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec info() -> list().
info() ->
    gen_server:call(?MODULE, info).

-spec set(any(), any()) -> 'ok'.
set(Key, Value) ->
    gen_server:cast(?SERVER, {set, Key, Value, infinity}).

-spec sync_set(any(), any()) -> any().
sync_set(Key, Value) ->
    gen_server:call(?SERVER, {set, Key, Value, infinity}).

-spec set(any(), any(), simple_cache:expire()) -> 'ok'.
set(Key, Value, Expires) ->
    gen_server:cast(?SERVER, {set, Key, Value, Expires}).

-spec sync_set(any(), any(), simple_cache:expire()) -> any().
sync_set(Key, Value, Expires) ->
    gen_server:call(?SERVER, {set, Key, Value, Expires}).

-spec lookup(any()) -> {'error','not_found'} | {'ok',_}.
lookup(Key) -> get_by_key(?SERVER, Key).

-spec lookup(any(), any()) -> {'ok',_}.
lookup(Key, Default) ->
    case lookup(Key) of
        {ok, Value} ->
            {ok, Value};
        {error, not_found} ->
            {ok, Default}
    end.

-spec flush(any()) -> 'ok'.
flush(Key) ->
    gen_server:cast(?SERVER, {flush, Key}),
    ok.

-spec sync_flush(any()) -> 'ok'.
sync_flush(Key) ->
    gen_server:call(?SERVER, {flush, Key}),
    ok.

-spec flush() -> 'ok'.
flush() ->
    gen_server:cast(?SERVER, flush),
    ok.

%%=============================================================================
%% gen_server Function Definitions
%%=============================================================================
init([]) ->
    {ok, #state{table = ets:new(?SERVER, ?ETS_OPTIONS)}}.

handle_call(info, _From,  #state{table = Table} = State) ->
    {reply, ets:info(Table), State};
handle_call({set, Key, Value, infinity}, _From, #state{table = Table} = State) ->
    ets:insert(Table, {Key, Value}),
    {reply, ok, State};
handle_call({set, Key, Value, Expires}, _From, #state{table = Table} = State) ->
    ets:insert(Table, {Key, Value}),
    erlang:send_after(1000 * Expires, ?SERVER, {expire, Key}),
    {reply, ok, State};
handle_call({flush, Key}, _From, #state{table = Table} = State) ->
    ets:delete(Table, Key),
    {reply, ok, State}.

handle_cast({set, Key, Value, infinity}, #state{table = Table} = State) ->
    ets:insert(Table, {Key, Value}),
    {noreply, State};
handle_cast({set, Key, Value, Expires}, #state{table = Table} = State) ->
    ets:insert(Table, {Key, Value}),
    erlang:send_after(1000 * Expires, ?SERVER, {expire, Key}),
    {noreply, State};
handle_cast({flush, Key}, #state{table = Table} = State) ->
    ets:delete(Table, Key),
    {noreply, State};
handle_cast(flush, #state{table = Table} = State) ->
    ets:delete_all_objects(Table),
    {noreply, State}.

handle_info({expire, Key}, #state{table = Table} = State) ->
    ets:delete(Table, Key),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%=============================================================================
%% Internal functionality
%%=============================================================================
get_by_key(Table, Key) ->
     case ets:lookup(Table, Key) of
        [{Key, Value}] ->
            {ok, Value};
        [] ->
            {error, not_found}
    end.