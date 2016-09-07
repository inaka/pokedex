-module(poke_model_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
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

-spec crud(config()) -> {comment, []}.
crud(_Config) ->
  ct:comment("Clean up pokedex"),
  _ = sumo:delete_all(pokemons),

  ct:comment("Capture a pokemon, use default name"),
  Bulbasaur = poke_pokemons_repo:capture(<<"Bulbasaur">>, 90, 38, 7.35, 0.69),

  ct:comment("We only have Bulbasaur"),
  [Bulbasaur] = poke_pokemons_repo:all(),

  ct:comment("It has proper default values"),
  <<"Bulbasaur">> = poke_pokemons:name(Bulbasaur),
  38 = poke_pokemons:total_hp(Bulbasaur),

  ct:comment("Change the name"),
  Luke = poke_pokemons_repo:update(poke_pokemons:name(Bulbasaur, <<"Luke">>)),
  <<"Luke">> = poke_pokemons:name(Luke),
  <<"Bulbasaur">> = poke_pokemons:species(Luke),

  ct:comment("Still just one pokemon"),
  [Luke] = poke_pokemons_repo:all(),

  ct:comment("New pokemon with name added"),
  Kali =
    poke_pokemons_repo:capture(<<"Kali">>, <<"Pikachu">>, 234, 100, 2.1, 3.0),
  <<"Kali">> = poke_pokemons:name(Kali),
  <<"Pikachu">> = poke_pokemons:species(Kali),

  ct:comment("Now we have 2 pokemons"),
  [] = poke_pokemons_repo:all() -- [Kali, Luke],

  ct:comment("Delete a pokemon"),
  true = poke_pokemons_repo:delete(poke_pokemons:id(Luke)),

  ct:comment("Only one pokemon left"),
  [Kali] = poke_pokemons_repo:all(),

  ct:comment("Delete a pokemon (again)"),
  false = poke_pokemons_repo:delete(poke_pokemons:id(Luke)),

  ct:comment("Still one pokemon left"),
  [Kali] = poke_pokemons_repo:all(),

  ct:comment("Delete the last pokemon"),
  true = poke_pokemons_repo:delete(poke_pokemons:id(Kali)),

  ct:comment("No pokemons left"),
  [] = poke_pokemons_repo:all(),

  {comment, ""}.
