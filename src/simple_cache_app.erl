-module(simple_cache_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%=============================================================================
%% Application callbacks
%%=============================================================================
-spec start(any(), term()) -> {'error', term()} | {'ok', pid()}.
start(_StartType, _StartArgs) ->
    simple_cache_sup:start_link().

-spec stop(atom()) -> 'ok'.
stop(_State) ->
    ok.
