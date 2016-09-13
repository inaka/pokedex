-module(poke_api_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([pokemon_crud/1, pokemon_errors/1]).
-export([species_crud/1, species_errors/1]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [ pokemon_crud
         , pokemon_errors
         , species_crud
         , species_errors
         ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _} = poke:start(),
  {ok, _} = application:ensure_all_started(hackney),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = application:stop(hackney),
  ok = poke:stop(),
  Config.

-spec pokemon_crud(config()) -> {comment, []}.
pokemon_crud(_Config) ->
  ct:comment("Clean up pokedex"),
  _ = sumo:delete_all(pokemons),

  ct:comment("Capture a pokemon, use default name"),
  BulbaJson = #{ species => <<"Bulbasaur">>
               , cp => 90
               , hp => 38
               , height => 7.35
               , weight => 0.69
               },
  {201, Bulbasaur} = api_call(post, "/pokemons", BulbaJson),

  ct:comment("We only have Bulbasaur"),
  {200, [Bulbasaur]} = api_call(get, "/pokemons"),

  ct:comment("It has proper default values"),
  #{ <<"name">> := <<"Bulbasaur">>
   , <<"total_hp">> := 38
   , <<"id">> := BulbasaurId
   } = Bulbasaur,

  ct:comment("Change the name"),
  LukeJson = #{name => <<"Luke">>},
  LukeUrl = <<"/pokemons/", BulbasaurId/binary>>,
  {200, Luke} = api_call(patch, LukeUrl, LukeJson),
  #{ <<"name">> := <<"Luke">>
   , <<"species">> := <<"Bulbasaur">>
   , <<"id">> := BulbasaurId
   } = Luke,

  ct:comment("We can get it by id"),
  {200, Luke} = api_call(get, LukeUrl),

  ct:comment("Still just one pokemon"),
  {200, [Luke]} = api_call(get, "/pokemons"),

  ct:comment("New pokemon with name added"),
  KaliJson = #{ species => <<"Pikachu">>
              , name => <<"Kali">>
              , cp => 234
              , hp => 100
              , height => 2.1
              , weight => 3.0
              },
  {201, Kali} = api_call(post, "/pokemons", KaliJson),
  #{ <<"name">> := <<"Kali">>
   , <<"species">> := <<"Pikachu">>
   , <<"id">> := KaliId
   } = Kali,

  ct:comment("Now we have 2 pokemons"),
  {200, [_, _]} = api_call(get, "/pokemons"),

  ct:comment("Delete a pokemon"),
  204 = api_call(delete, LukeUrl),

  ct:comment("No longer there"),
  404 = api_call(get, LukeUrl),

  ct:comment("Only one pokemon left"),
  {200, [Kali]} = api_call(get, "/pokemons"),

  ct:comment("Delete a pokemon (again)"),
  404 = api_call(delete, LukeUrl),

  ct:comment("Still one pokemon left"),
  {200, [Kali]} = api_call(get, "/pokemons"),

  ct:comment("Delete the last pokemon"),
  KaliUrl = <<"/pokemons/", KaliId/binary>>,
  204 = api_call(delete, KaliUrl),

  ct:comment("No pokemons left"),
  {200, []} = api_call(get, "/pokemons"),

  {comment, ""}.

-spec pokemon_errors(config()) -> {comment, []}.
pokemon_errors(_Config) ->
  ct:comment("Clean up pokedex and create one"),
  _ = sumo:delete_all(pokemons),
  Ekans =
    sumo:persist(
      pokemons,
      poke_pokemons:new(<<"Ekans">>, <<"Ekans">>, 10, 20, 20, 3.0, 4.0)),
  EkansId = poke_pokemons:id(Ekans),
  EkansUrl = <<"/pokemons/", EkansId/binary>>,

  ct:comment("Bad format"),
  Hdrs = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
  415 = api_call(post, "/pokemons", Hdrs, {form, ""}),
  415 = api_call(patch, EkansUrl, Hdrs, {form, ""}),

  ct:comment("Bad json"),
  {400, _} = api_call(post, "/pokemons", <<"{">>),
  {400, _} = api_call(patch, EkansUrl, <<"{">>),

  ct:comment("Missing fields"),
  {400, _} = api_call(post, "/pokemons", #{}),
  {400, _} = api_call(post, "/pokemons", #{species => <<"Ekans">>}),
  {400, _} = api_call(post, "/pokemons", #{ species => <<"Ekans">>
                                          , cp => 10
                                          }),
  {400, _} = api_call(post, "/pokemons", #{ species => <<"Ekans">>
                                          , cp => 10
                                          , hp => 20
                                          }),
  {400, _} = api_call(post, "/pokemons", #{ species => <<"Ekans">>
                                          , cp => 10
                                          , hp => 20
                                          , height => 3.0
                                          }),

  ct:comment("Wrong accept header"),
  406 = api_call(post, "/pokemons", [{<<"Accept">>, <<"text/html">>}], <<>>),
  406 = api_call(get, "/pokemons", [{<<"Accept">>, <<"text/html">>}]),
  406 = api_call(patch, EkansUrl, [{<<"Accept">>, <<"text/html">>}], <<>>),
  406 = api_call(get, EkansUrl, [{<<"Accept">>, <<"text/html">>}]),

  {comment, ""}.

-spec species_crud(config()) -> {comment, []}.
species_crud(_Config) ->
  ct:comment("Clean up species list"),
  _ = sumo:delete_all(species),

  ct:comment("Register a species"),
  OddishJson = #{ name => <<"oddish">>
                , types => [<<"grass">>, <<"poison">>]
                },
  {201, Oddish} = api_call(post, "/species", OddishJson),

  ct:comment("We only have Oddish"),
  {200, [Oddish]} = api_call(get, "/species"),

  ct:comment("It has proper values"),
  #{ <<"name">> := <<"oddish">>
   , <<"types">> := [<<"grass">>, <<"poison">>]
   } = Oddish,

  ct:comment("Change the types"),
  Oddish2Json = #{types => [<<"grass">>]},
  {200, Oddish2} = api_call(patch, "/species/oddish", Oddish2Json),
  #{ <<"name">> := <<"oddish">>
   , <<"types">> := [<<"grass">>]
   } = Oddish2,

  ct:comment("We can get it by name"),
  {200, Oddish2} = api_call(get, "/species/oddish"),

  ct:comment("Still just one"),
  {200, [Oddish2]} = api_call(get, "/species"),

  ct:comment("New pokemon with name added"),
  PikaJson = #{ name => <<"pikachu">>
              , types => [<<"electric">>]
              },
  {201, Pika} = api_call(post, "/species", PikaJson),
  #{ <<"name">> := <<"pikachu">>
   , <<"types">> := [<<"electric">>]
   } = Pika,

  ct:comment("Now we have 2 species"),
  {200, [_, _]} = api_call(get, "/species"),

  ct:comment("Delete a species"),
  204 = api_call(delete, "/species/oddish"),

  ct:comment("No longer there"),
  404 = api_call(get, "/species/oddish"),

  ct:comment("Only one left"),
  {200, [Pika]} = api_call(get, "/species"),

  ct:comment("Delete a species (again)"),
  404 = api_call(delete, "/species/oddish"),

  ct:comment("Still one left"),
  {200, [Pika]} = api_call(get, "/species"),

  ct:comment("Delete the last one"),
  204 = api_call(delete, "/species/pikachu"),

  ct:comment("No species left"),
  {200, []} = api_call(get, "/species"),

  {comment, ""}.

-spec species_errors(config()) -> {comment, []}.
species_errors(_Config) ->
  ct:comment("Clean up species and create one"),
  _ = sumo:delete_all(species),
  _ = sumo:persist(species, poke_species:new(<<"weedle">>, [<<"bug">>])),

  ct:comment("Bad format"),
  Hdrs = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
  415 = api_call(post, "/species", Hdrs, {form, ""}),
  415 = api_call(patch, "/species/weedle", Hdrs, {form, ""}),

  ct:comment("Bad json"),
  {400, #{<<"error">> := <<"Malformed JSON request">>}} =
    api_call(post, "/species", <<"{">>),
  {400, #{<<"error">> := <<"Malformed JSON request">>}} =
    api_call(patch, "/species/weedle", <<"{">>),

  ct:comment("Missing fields"),
  {400, _} = api_call(post, "/species", #{}),
  {400, _} = api_call(post, "/species", #{name => <<"pikachu">>}),

  ct:comment("Wrong values"),
  {400, _} = api_call(post, "/species", #{name => <<"pikachu">>, types => []}),
  {400, _} =
    api_call(
      post, "/species",
      #{name => <<"pikachu">>, types => [<<"a">>, <<"b">>, <<"c">>]}),
  {400, _} = api_call(patch, "/species/weedle", #{types => []}),
  {400, _} =
    api_call(patch, "/species/weedle", #{types => [<<"a">>, <<"b">>, <<"c">>]}),

  ct:comment("Wrong accept header"),
  406 = api_call(post, "/species", [{<<"Accept">>, <<"text/html">>}], <<>>),
  406 = api_call(get, "/species", [{<<"Accept">>, <<"text/html">>}]),
  406 =
    api_call(patch, "/species/weedle", [{<<"Accept">>, <<"text/html">>}], <<>>),
  406 = api_call(get, "/species/weedle", [{<<"Accept">>, <<"text/html">>}]),

  ct:comment("Conflict"),
  {409, _} =
    api_call(post, "/species", #{name => <<"weedle">>, types => [<<"bug">>]}),

  {comment, ""}.

api_call(Method, Path) ->
  api_call(Method, Path, []).

api_call(Method, Path, Hdrs) when is_list(Hdrs) ->
  api_call(Method, Path, Hdrs, <<>>);
api_call(Method, Path, Body) ->
  api_call(Method, Path, [{<<"Content-Type">>, <<"application/json">>}], Body).

api_call(Method, Path, Hdrs, Json) when is_map(Json) ->
  api_call(Method, Path, Hdrs, jsx:encode(Json));
api_call(Method, Path, Hdrs, Body) ->
  Port = integer_to_binary(application:get_env(pokedex, http_port, 8080)),
  BinPath = iolist_to_binary(Path),
  Url = <<"http://localhost:", Port/binary, BinPath/binary>>,
  ct:log("~p ~p -d '~p'", [Method, Url, Body]),
  try hackney:request(Method, Url, Hdrs, Body) of
    {ok, Status, _RespHdrs} -> Status;
    {ok, Status, _RespHdrs, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, <<>>} -> Status;
        {ok, RespBody} ->
          ct:log(">>> ~p", [RespBody]),
          {Status, maybe_decode(RespBody)}
      end;
    {error, Error} ->
      ct:fail("Couldnt ~p ~p: ~p", [Method, Path, Error])
  catch
    _:X ->
      ct:log("Error: ~p; Stack: ~p", [X, erlang:get_stacktrace()]),
      ct:fail("Couldnt ~p ~p: ~p", [Method, Path, X])
  end.

%% @todo remove once inaka/sumo_rest#44 is fixed
maybe_decode(RespBody) ->
  try jsx:decode(RespBody, [return_maps])
  catch
    _:_ -> RespBody
  end.
