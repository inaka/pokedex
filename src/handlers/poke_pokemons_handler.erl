%%% @doc GET|POST /pokemons implementation
-module(poke_pokemons_handler).

-behaviour(trails_handler).

-type state() :: #{}.

-export([ init/3
        , allowed_methods/2
        , resource_exists/2
        , content_types_accepted/2
        , content_types_provided/2
        , handle_get/2
        , handle_post/2
        ]).

-export([trails/0]).

-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"a pokemon">>
     , required => true
     , schema =>
         #{ <<"type">> => <<"object">>
          , <<"required">> => [ <<"species">>
                              , <<"cp">>
                              , <<"hp">>
                              , <<"height">>
                              , <<"weight">>
                              ]
          , <<"properties">> =>
              #{ <<"species">> => #{<<"type">> => <<"string">>}
               , <<"name">> => #{<<"type">> => <<"string">>}
               , <<"cp">> => #{<<"type">> => <<"integer">>}
               , <<"hp">> => #{<<"type">> => <<"integer">>}
               , <<"height">> => #{<<"type">> => <<"float">>}
               , <<"weight">> => #{<<"type">> => <<"float">>}
               }
          }
     },
  PostResponses =
    #{ <<"201">> =>
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
     },
  GetResponses =
    #{ <<"200">> =>
         #{ description => <<"a list of pokemons">>
          , schema =>
              #{ type => <<"array">>
               , items =>
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
          }
     },
  Metadata =
    #{ get =>
       #{ tags => ["pokemons"]
        , description => "Returns the list of pokemons"
        , produces => ["application/json"]
        , responses => GetResponses
        }
     , post =>
       # { tags => ["pokemons"]
         , description => "Creates a new pokemon"
         , consumes => ["application/json"]
         , produces => ["application/json"]
         , parameters => [RequestBody]
         , responses => PostResponses
         }
     },
  [trails:trail("/pokemons", ?MODULE, #{}, Metadata)].

%% @private
%% @see cowboy_rest:init/3
-spec init({atom(), atom()}, cowboy_req:req(), _) ->
  {upgrade, protocol, cowboy_rest}.
init(_Transport, Req, _Opts) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path,   _} = cowboy_req:path(Req1),
  _ = error_logger:info_msg("~s ~s", [Method, Path]),
  {upgrade, protocol, cowboy_rest}.

%% @private
%% @see cowboy_rest:allowed_methods/2
-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>], Req, State}.

%% @private
%% @see cowboy_rest:resource_exists/2
-spec resource_exists(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Method =/= <<"POST">>, Req1, State}.

%% @pprivate
%% @see cowboy_rest:content_types_accepted/2
-spec content_types_accepted(cowboy_req:req(), state()) ->
  {[{{binary(), binary(), '*'}, atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}.

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
  Reply = [poke_pokemons:to_json(P) || P <- poke_pokemons_repo:all()],
  JSON  = jsx:encode(Reply),
  {JSON, Req, State}.

%% @doc Registers a captured pokemon.
-spec handle_post(cowboy_req:req(), state()) ->
  {{true, binary()} | false | halt, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json             = jsx:decode(Body, [return_maps]),
    case poke_pokemons:from_json(Json) of
      {error, Reason} ->
        Req2 = cowboy_req:set_resp_body(jsx:encode(Reason), Req1),
        {false, Req2, State};
      {ok, Pokemon} ->
        PersistedPokemon = poke_pokemons_repo:capture(Pokemon),
        ResBody = jsx:encode(poke_pokemons:to_json(PersistedPokemon)),
        Req2 = cowboy_req:set_resp_body(ResBody, Req1),
        Id = poke_pokemons:id(PersistedPokemon),
        {{true, <<"/pokemons/", Id/binary>>}, Req2, State}
    end
  catch
    _:badarg ->
      Req3 =
        cowboy_req:set_resp_body(jsx:encode(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end.
