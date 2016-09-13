-module(poke_species).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type name() :: binary().
-type type() :: binary().

-opaque species() ::
  #{ name => name()
   , types => [type()]
   , created_at => calendar:datetime()
   , updated_at => calendar:datetime()
   }.

-type updates() :: map().

-export_type(
  [ name/0
  , type/0
  , species/0
  , updates/0
  ]).

-export(
  [ sumo_schema/0
  , sumo_sleep/1
  , sumo_wakeup/1
  ]).

-export(
  [ new/2
  , id/1
  , to_json/1
  , from_json/1
  , update/2
  , location/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    species,
    [ sumo:new_field(name,        string,   [id, unique])
    , sumo:new_field(types,       custom,   [{type, list}])
    , sumo:new_field(created_at,  datetime, [not_null])
    , sumo:new_field(updated_at,  datetime, [not_null])
    ]).

-spec sumo_sleep(species()) -> sumo:model().
sumo_sleep(Species) -> Species.

-spec sumo_wakeup(sumo:model()) -> species().
sumo_wakeup(Species) -> Species.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new(name(), [type()]) -> species().
new(Name, Types) ->
  #{ name       => Name
   , types      => Types
   , created_at => calendar:universal_time()
   , updated_at => calendar:universal_time()
   }.

-spec id(species()) -> name().
id(#{name := Name}) -> Name.

-spec to_json(species()) -> sr_json:json().
to_json(Species) ->
  #{ name       => maps:get(name, Species)
   , types      => maps:get(types, Species)
   }.

-spec from_json(sr_json:json()) -> {ok, species()} | {error, binary()}.
from_json(Json) ->
  try
    Name = maps:get(<<"name">>, Json),
    Types = maps:get(<<"types">>, Json),
    check_types(Types),
    {ok, new(Name, Types)}
  catch
    _:{invalid_types, InvalidTypes} ->
      {error, <<"Invalid types: ", InvalidTypes/binary>>};
    _:{badkey, Key} ->
      {error, <<"Missing field: ", Key/binary>>}
  end.

-spec update(species(), updates()) -> {ok, species()} | {error, binary()}.
update(Species, Updates) ->
  try
    Types = maps:get(<<"types">>, Updates, maps:get(types, Species)),
    check_types(Types),
    {ok, Species#{ types := Types
                 , updated_at := calendar:universal_time()
                 }}
  catch
    _:{invalid_types, InvalidTypes} ->
      {error, <<"Invalid types: ", InvalidTypes/binary>>}
  end.

-spec location(species(), sumo_rest_doc:path()) -> iodata().
location(#{name := Name}, Root) -> [Root, $/, Name].

check_types([_]) -> valid;
check_types([_, _]) -> valid;
check_types(InvalidTypes) ->
  throw({invalid_types, iolist_to_binary(jsx:encode(InvalidTypes))}).
