-module(poke_api_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([crud/1]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [crud].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _} = poke:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = poke:stop(),
  Config.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_, Config) ->
  ct:comment("Clean up pokedex"),
  _ = sumo:delete_all(pokemons),

  ct:comment("Connect to server"),
  Port = application:get_env(pokenaka, http_port, 8080),
  {ok, Pid} = fusco:start("http://localhost:" ++ integer_to_list(Port), []),
  ok = fusco:connect(Pid),
  [{fusco_client, Pid} | Config].

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(_, Config) ->
  {value, {fusco_client, Pid}, NewConfig} =
    lists:keytake(fusco_client, 1, Config),
  ok = fusco:disconnect(Pid),
  ok = fusco:stop(Pid),
  NewConfig.

-spec crud(config()) -> {comment, []}.
crud(Config) ->
  ct:comment("Capture a pokemon, use default name"),
  BulbaJson = #{ species => <<"Bulbasaur">>
               , cp => 90
               , hp => 38
               , height => 7.35
               , weight => 0.69
               },
  {201, Bulbasaur} = api_call(Config, "/pokemons", post, BulbaJson),

  ct:comment("We only have Bulbasaur"),
  {200, [Bulbasaur]} = all(Config),

  ct:comment("It has proper default values"),
  #{ <<"name">> := <<"Bulbasaur">>
   , <<"total_hp">> := 38
   , <<"id">> := BulbasaurId
   } = Bulbasaur,

  ct:comment("Change the name"),
  LukeJson = #{name => <<"Luke">>},
  LukeUrl = <<"/pokemons/", BulbasaurId/binary>>,
  {200, Luke} = api_call(Config, LukeUrl, patch, LukeJson),
  #{ <<"name">> := <<"Luke">>
   , <<"species">> := <<"Bulbasaur">>
   , <<"id">> := BulbasaurId
   } = Luke,

  ct:comment("Still just one pokemon"),
  {200, [Luke]} = all(Config),

  ct:comment("New pokemon with name added"),
  KaliJson = #{ species => <<"Pikachu">>
              , name => <<"Kali">>
              , cp => 234
              , hp => 100
              , height => 2.1
              , weight => 3.0
              },
  {201, Kali} = api_call(Config, "/pokemons", post, KaliJson),
  #{ <<"name">> := <<"Kali">>
   , <<"species">> := <<"Pikachu">>
   , <<"id">> := KaliId
   } = Kali,

  ct:comment("Now we have 2 pokemons"),
  {200, [_, _]} = all(Config),

  ct:comment("Delete a pokemon"),
  204 = api_call(Config, LukeUrl, delete),

  ct:comment("Only one pokemon left"),
  {200, [Kali]} = all(Config),

  ct:comment("Delete a pokemon (again)"),
  204 = api_call(Config, LukeUrl, delete),

  ct:comment("Still one pokemon left"),
  {200, [Kali]} = all(Config),

  ct:comment("Delete the last pokemon"),
  KaliUrl = <<"/pokemons/", KaliId/binary>>,
  204 = api_call(Config, KaliUrl, delete),

  ct:comment("No pokemons left"),
  {200, []} = all(Config),

  {comment, ""}.

api_call(Config, Path, Method) ->
  api_call(Config, Path, Method, []).

api_call(Config, Path, Method, Hdrs) when is_list(Hdrs) ->
  api_call(Config, Path, Method, Hdrs, <<>>);
api_call(Config, Path, Method, Body) ->
  api_call(Config, Path, Method, [], Body).

api_call(Config, Path, Method, Hdrs, Json) when is_map(Json) ->
  api_call(Config, Path, Method, Hdrs, jsx:encode(Json));
api_call(Config, Path, Method, Hdrs, Body) ->
  {fusco_client, Client} = lists:keyfind(fusco_client, 1, Config),
  try fusco:request(Client, Path, Method, Hdrs, Body, 1000) of
    {ok, {{Status, _}, _RespHdrs, _Body, 0, _Timeout}} ->
      binary_to_integer(Status);
    {ok, {{Status, _}, _RespHdrs, Body, _Size, _Timeout}} ->
      {binary_to_integer(Status), jsx:decode(Body)}
  catch
    _:X ->
      ct:log("Error: ~p; Stack: ~p", [X, erlang:get_stacktrace()]),
      ct:fail("Couldnt ~p ~p: ~p", [Method, Path, X])
  end.

all(Config) -> api_call(Config, "/pokemons", get).
