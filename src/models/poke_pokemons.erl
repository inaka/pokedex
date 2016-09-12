-module(poke_pokemons).

-behaviour(sumo_doc).

-type id() :: binary().
-type name() :: binary().
-type species() :: binary().

-opaque pokemon() ::
  #{ id         => id()
   , name       := name()
   , species    := species()
   , cp         := non_neg_integer()
   , hp         := non_neg_integer()
   , total_hp   := pos_integer()
   , height     := float()
   , weight     := float()
   , created_at := calendar:datetime()
   , updated_at := calendar:datetime()
   }.

-type updates() :: map().

-export_type(
  [ id/0
  , name/0
  , species/0
  , pokemon/0
  , updates/0
  ]).

%% sumo_doc behaviour callbacks
-export(
  [ sumo_schema/0
  , sumo_sleep/1
  , sumo_wakeup/1
  ]).

-export(
  [ new/7
  , id/1
  , to_json/1
  , from_json/1
  , update/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    pokemons,
    [ sumo:new_field(id,          binary,   [id, unique])
    , sumo:new_field(name,        string,   [not_null])
    , sumo:new_field(species,     string,   [not_null])
    , sumo:new_field(cp,          integer,  [not_null])
    , sumo:new_field(hp,          integer,  [not_null])
    , sumo:new_field(total_hp,    integer,  [not_null])
    , sumo:new_field(height,      float,    [not_null])
    , sumo:new_field(weight,      float,    [not_null])
    , sumo:new_field(created_at,  datetime, [not_null])
    , sumo:new_field(updated_at,  datetime, [not_null])
    ]).

-spec sumo_sleep(pokemon()) -> sumo:model().
sumo_sleep(Pokemon) -> Pokemon.

-spec sumo_wakeup(sumo:model()) -> pokemon().
sumo_wakeup(Pokemon) -> Pokemon.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new(
  name(), species(), non_neg_integer(), non_neg_integer(),
  pos_integer(), float(), float()) -> pokemon().
new(Name, Species, CP, HP, TotalHP, Height, Weight) ->
  #{ name       => Name
   , species    => Species
   , cp         => CP
   , hp         => HP
   , total_hp   => TotalHP
   , height     => Height
   , weight     => Weight
   , created_at => calendar:universal_time()
   , updated_at => calendar:universal_time()
   }.

-spec id(pokemon()) -> id().
id(#{id := Id}) -> Id.

-spec to_json(pokemon()) -> map().
to_json(Pokemon) ->
  #{ id         => maps:get(id, Pokemon, null)
   , name       => maps:get(name, Pokemon)
   , species    => maps:get(species, Pokemon)
   , cp         => maps:get(cp, Pokemon)
   , hp         => maps:get(hp, Pokemon)
   , total_hp   => maps:get(total_hp, Pokemon)
   , height     => maps:get(height, Pokemon)
   , weight     => maps:get(weight, Pokemon)
   }.

-spec from_json(map()) -> {ok, pokemon()} | {error, binary()}.
from_json(Json) ->
  try
    Species = maps:get(<<"species">>, Json),
    Name = maps:get(<<"name">>, Json, Species),
    CP = maps:get(<<"cp">>, Json),
    HP = maps:get(<<"hp">>, Json),
    Height = maps:get(<<"height">>, Json),
    Weight = maps:get(<<"weight">>, Json),
    {ok, new(Name, Species, CP, HP, HP, Height, Weight)}
  catch
    _:{badkey, Key} ->
      {error, <<"Missing field: ", Key/binary>>}
  end.

-spec update(pokemon(), updates()) -> pokemon().
update(Pokemon, Updates) ->
  Name = maps:get(<<"name">>, Updates, maps:get(name, Pokemon)),
  Pokemon#{ name := Name
          , updated_at := calendar:universal_time()
          }.
