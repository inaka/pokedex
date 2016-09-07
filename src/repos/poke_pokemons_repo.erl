-module(poke_pokemons_repo).

-export(
  [ capture/5
  , capture/6
  , all/0
  , update/1
  , delete/1
  ]).

-spec capture(
  poke_pokemons:species(), non_neg_integer(), non_neg_integer(),
  float(), float()) -> poke_pokemons:pokemon().
capture(Species, CP, HP, Height, Weight) ->
  capture(Species, Species, CP, HP, Height, Weight).

-spec capture(
  poke_pokemons:name(), poke_pokemons:species(), non_neg_integer(),
  non_neg_integer(), float(), float()) -> poke_pokemons:pokemon().
capture(Name, Species, CP, HP, Height, Weight) ->
  sumo:persist(
    pokemons, poke_pokemons:new(Name, Species, CP, HP, HP, Height, Weight)).

-spec all() -> [poke_pokemons:pokemon()].
all() -> sumo:find_all(pokemons).

-spec update(poke_pokemons:pokemon()) -> poke_pokemons:pokemon().
update(Pokemon) -> sumo:persist(pokemons, Pokemon).

-spec delete(poke_pokemons:id()) -> boolean().
delete(PokemonId) -> sumo:delete(pokemons, PokemonId).
