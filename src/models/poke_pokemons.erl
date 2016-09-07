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

-export_type(
  [ id/0
  , name/0
  , species/0
  , pokemon/0
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
  , name/1
  , species/1
  , total_hp/1
  , name/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    pokemons,
    [ sumo:new_field(id,          string,   [id, unique])
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

-spec name(pokemon()) -> name().
name(#{name := Name}) -> Name.

-spec species(pokemon()) -> species().
species(#{species := Species}) -> Species.

-spec total_hp(pokemon()) -> pos_integer().
total_hp(#{total_hp := TotalHP}) -> TotalHP.

-spec name(pokemon(), name()) -> pokemon().
name(Pokemon, Name) ->
  Pokemon#{name := Name, updated_at := calendar:universal_time()}.

