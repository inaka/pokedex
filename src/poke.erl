-module(poke).

-behaviour(application).

-export([ start/0
        , start/2
        , stop/0
        , stop/1
        ]).

%% @doc Starts the app
-spec start() -> {ok, [atom()]}.
start() -> application:ensure_all_started(pokenaka).

%% @doc Stops the app
-spec stop() -> ok.
stop() -> application:stop(pokenaka).

%% @private
-spec start(application:start_type(), noargs) -> {ok, pid()}.
start(_Type, noargs) -> {ok, self()}.

%% @private
-spec stop([]) -> ok.
stop([]) -> ok.
