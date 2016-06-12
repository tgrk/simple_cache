-module(simple_cache_server).

-behaviour(gen_server).

-define(SERVER,      ?MODULE).
-define(ETS_OPTIONS, [{read_concurrency, true},
                      {write_concurrency, true},
                      named_table,
                      protected
                     ]).

-record(state, {table, options}).

%%=============================================================================
%% API Function Exports
%%=============================================================================
-export([start_link/0,
         start_link/1,
         ops_info/0,
         ops_list/0,
         sync_set/2,
         set/2,
         sync_set/3,
         set/3,
         cond_set/4,
         lookup/1,
         lookup/2,
         flush/1,
         sync_flush/1,
         flush/0,
         sync_flush/0
        ]).

%%=============================================================================
%% gen_server Function Exports
%%=============================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%=============================================================================
%% API Function Definitions
%%=============================================================================
-spec start_link() -> 'ignore' | {'error', term()} | {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_link(list({atom(), any()}))
                -> 'ignore' | {'error', term()} | {'ok', pid()}.
start_link(CustomOptions) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [CustomOptions], []).

-spec ops_info() -> list().
ops_info() ->
    gen_server:call(?MODULE, ops_info).

-spec ops_list() -> list().
ops_list() ->
    gen_server:call(?MODULE, ops_list).

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

-spec cond_set(any(), any(), simple_cache:conditional(),
               simple_cache:expire()) -> {ok, boolean()}.
cond_set(Key, Value, Conditional, Expires) ->
    gen_server:call(?SERVER, {set, Key, Value, Conditional, Expires}).

-spec lookup(any()) -> {'error','not_found'} | {'ok', any()}.
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

-spec sync_flush() -> 'ok'.
sync_flush() ->
    gen_server:call(?SERVER, flush).

%%=============================================================================
%% gen_server Function Definitions
%%=============================================================================
init([]) ->
    {ok, #state{table = ets:new(?SERVER, ?ETS_OPTIONS),
                options = ?ETS_OPTIONS}};
init(CustomOptions) ->
    Options = merge_options(?ETS_OPTIONS, CustomOptions),
    {ok, #state{table = ets:new(?SERVER, Options),
                options = Options}}.

handle_call(ops_info, _From,  #state{table = Table} = State) ->
    {reply, ets:info(Table), State};
handle_call(ops_list, _From,  #state{table = Table} = State) ->
    {reply, ets:tab2list(Table), State};
handle_call({set, Key, Value, infinity}, _From, #state{table = Table} = State) ->
    insert(Table, Key, Value, infinity),
    {reply, ok, State};
handle_call({set, Key, Value, Expires}, _From, #state{table = Table} = State) ->
    insert(Table, Key, Value, Expires),
    {reply, ok, State};
handle_call({set, Key, Value, Conditional, Expires}, _From, #state{table = Table} = State) ->
    Test = case lookup(Key) of
               {ok, OldValue}     -> Conditional(OldValue);
               {error, not_found} -> true
           end,
    Test andalso insert(Table, Key, Value, Expires),
    {reply, {ok, Test}, State};
handle_call({flush, Key}, _From, #state{table = Table} = State) ->
    ets:delete(Table, Key),
    {reply, ok, State};
handle_call(flush, _From, #state{table = Table} = State) ->
    true = ets:delete_all_objects(Table),
    {reply, ok, State}.

handle_cast({set, Key, Value, infinity}, #state{table = Table} = State) ->
    insert(Table, Key, Value, infinity),
    {noreply, State};
handle_cast({set, Key, Value, Expires}, #state{table = Table} = State) ->
    insert(Table, Key, Value, Expires),
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
insert(Table, Key, Value, infinity) ->
    ets:insert(Table, {Key, Value, infinity});
insert(Table, Key, Value, Expires) ->
    ets:insert(Table, {Key, Value, Expires}),
    erlang:send_after(1000 * Expires, ?SERVER, {expire, Key}).

get_by_key(Table, Key) ->
     case ets:lookup(Table, Key) of
        [{Key, Value, _Expires}] ->
            {ok, Value};
        [] ->
            {error, not_found}
    end.

merge_options(ExistingOptions, NewOptions) ->
    orddict:merge(fun (_, X, Y) -> X + Y end,
                  orddict:from_list(ExistingOptions),
                  orddict:from_list(NewOptions)).
