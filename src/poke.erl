-module(poke).

-behaviour(application).

-export([ start/0
        , start/2
        , stop/0
        , stop/1
        , start_phase/3
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

%% @private
-spec start_phase(atom(), StartType::application:start_type(), []) ->
  ok | {error, _}.
start_phase(create_schema, _StartType, []) ->
  _ = application:stop(mnesia),
  Node = node(),
  case mnesia:create_schema([Node]) of
    ok -> ok;
    {error, {Node, {already_exists, Node}}} -> ok
  end,
  {ok, _} = application:ensure_all_started(mnesia),
  % Create persistency schema
  sumo:create_schema().
