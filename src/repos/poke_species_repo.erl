-module(poke_species_repo).

-export([exists/1]).

-spec exists(poke_species:name()) -> boolean().
exists(Name) -> notfound =/= sumo:find(species, Name).
