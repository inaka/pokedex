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
  sumo:create_schema();
start_phase(start_cowboy, _StartType, []) ->
  Port = application:get_env(pokenaka, http_port, 8080),
  ListenerCount = application:get_env(pokenaka, http_listener_count, 10),

  Handlers =
    [ poke_pokemons_handler
    , poke_single_pokemon_handler
    , cowboy_swagger_handler
    ],
  Trails = trails:trails(Handlers),
  trails:store(Trails),

  Dispatch = trails:single_host_compile(Trails),
  TransOpts = [{port, Port}],
  ProtoOpts = [{env, [{dispatch, Dispatch}, {compress, true}]}],

  case cowboy:start_http(pokenaka, ListenerCount, TransOpts, ProtoOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end.
