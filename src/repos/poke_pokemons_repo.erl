-module(poke_pokemons_repo).

-export(
  [ capture/1
  , all/0
  , update/1
  , delete/1
  , fetch/1
  ]).

-spec capture(poke_pokemons:pokemon()) -> poke_pokemons:pokemon().
capture(Pokemon) -> sumo:persist(pokemons, Pokemon).

-spec all() -> [poke_pokemons:pokemon()].
all() -> sumo:find_all(pokemons).

-spec update(poke_pokemons:pokemon()) -> poke_pokemons:pokemon().
update(Pokemon) -> sumo:persist(pokemons, Pokemon).

-spec delete(poke_pokemons:id()) -> boolean().
delete(PokemonId) -> sumo:delete(pokemons, PokemonId).

-spec fetch(poke_pokemons:id()) -> notfound | poke_pokemons:pokemon().
fetch(Id) -> sumo:find(pokemons, Id).
