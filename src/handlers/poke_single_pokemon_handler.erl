%%% @doc GET|PATCH|DELETE /pokemons/:id implementation
-module(poke_single_pokemon_handler).

-behaviour(trails_handler).

-type state() :: #{id => binary(), pokemon => poke_pokemons:pokemon()}.

-export([ init/3
        , rest_init/2
        , allowed_methods/2
        , resource_exists/2
        , content_types_accepted/2
        , content_types_provided/2
        , handle_get/2
        , handle_patch/2
        , delete_resource/2
        ]).

-export([trails/0]).

-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"fields to update">>
     , required => true
     , schema =>
         #{ <<"type">> => <<"object">>
          , <<"properties">> =>
              #{ <<"name">> => #{<<"type">> => <<"string">>}
               }
          }
     },
  Responses =
    #{ <<"200">> =>
         #{ description => <<"successful request with response body">>
          , schema =>
              #{ type => <<"object">>
               , properties =>
                  #{ <<"id">> => #{<<"type">> => <<"string">>}
                   , <<"species">> => #{<<"type">> => <<"string">>}
                   , <<"name">> => #{<<"type">> => <<"string">>}
                   , <<"cp">> => #{<<"type">> => <<"integer">>}
                   , <<"hp">> => #{<<"type">> => <<"integer">>}
                   , <<"height">> => #{<<"type">> => <<"float">>}
                   , <<"weight">> => #{<<"type">> => <<"float">>}
                   }
               }
          }
     , <<"400">> => #{description => <<"Invalid parameters">>}
     , <<"404">> => #{description => <<"Pokemon not found">>}
     },
  Metadata =
    #{ get =>
       #{ tags => ["pokemons"]
        , description => "Returns a pokemon"
        , produces => ["application/json"]
        , responses => Responses
        }
     , patch =>
       # { tags => ["newspapers"]
         , description => "Updates a pokemon"
         , consumes => ["application/json"]
         , produces => ["application/json"]
         , parameters => [RequestBody]
         , responses => Responses
         }
     , delete =>
       #{ tags => ["pokemons"]
        , description => "Deletes a pokemon"
        , produces => ["application/json"]
        , responses => Responses
        }
     },
  [trails:trail("/pokemons/:id", ?MODULE, #{}, Metadata)].

%% @private
%% @see cowboy_rest:init/3
-spec init({atom(), atom()}, cowboy_req:req(), _) ->
  {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% @private
%% @see cowboy_rest:rest_init/2
-spec rest_init(cowboy_req:req(), _) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, _Opts) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path,   Req2} = cowboy_req:path(Req1),
  _ = error_logger:info_msg("~s ~s", [Method, Path]),
  {Id, Req3} = cowboy_req:binding(id, Req2),
  {ok, Req3, #{id => Id}}.

%% @private
%% @see cowboy_rest:allowed_methods/2
-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"DELETE">>, <<"PATCH">>, <<"GET">>], Req, State}.

%% @private
%% @see cowboy_rest:resource_exists/2
-spec resource_exists(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  #{id := Id} = State,
  case poke_pokemons_repo:fetch(Id) of
    notfound -> {false, Req, State};
    Pokemon -> {true, Req, State#{pokemon => Pokemon}}
  end.

%% @pprivate
%% @see cowboy_rest:content_types_accepted/2
-spec content_types_accepted(cowboy_req:req(), state()) ->
  {[{{binary(), binary(), '*'}, atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_patch}], Req, State}.

%% @private
%% @see cowboy_rest:content_types_provided/2
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

%% @doc Returns the list of all pokemons.
-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  #{pokemon := Pokemon} = State,
  JSON  = jsx:encode(poke_pokemons:to_json(Pokemon)),
  {JSON, Req, State}.

%% @doc Updates the found pokemon.
-spec handle_patch(cowboy_req:req(), state()) ->
  {{true, binary()} | false | halt, cowboy_req:req(), state()}.
handle_patch(Req, #{pokemon := Pokemon} = State) ->
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json = jsx:decode(Body, [return_maps]),
    PersistedPokemon =
      poke_pokemons_repo:update(poke_pokemons:update(Pokemon, Json)),
    ResBody = jsx:encode(poke_pokemons:to_json(PersistedPokemon)),
    Req2 = cowboy_req:set_resp_body(ResBody, Req1),
    {true, Req2, State}
  catch
    _:badarg ->
      Req3 =
        cowboy_req:set_resp_body(
          jsx:encode(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end.

%% @doc Deletes the found pokemon.
-spec delete_resource(cowboy_req:req(), state()) ->
  {boolean() | halt, cowboy_req:req(), state()}.
delete_resource(Req, State) ->
  #{id := Id} = State,
  Result = poke_pokemons_repo:delete(Id),
  {Result, Req, State}.
