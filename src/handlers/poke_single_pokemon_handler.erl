%%% @doc GET|PATCH|DELETE /pokemons/:id implementation
-module(poke_single_pokemon_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , handle_patch/2
          , delete_resource/2
          ]
        }]).

-export([trails/0]).

-spec trails() -> trails:trails().
trails() ->
  Id =
    #{ name => id
     , description => <<"pokemon id">>
     , in => path
     , type => string
     , required => true
     },
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
        , parameters => [Id]
        , responses => Responses
        }
     , patch =>
       # { tags => ["pokemons"]
         , description => "Updates a pokemon"
         , consumes => ["application/json"]
         , produces => ["application/json"]
         , parameters => [RequestBody, Id]
         , responses => Responses
         }
     , delete =>
       #{ tags => ["pokemons"]
        , description => "Deletes a pokemon"
        , produces => ["application/json"]
        , parameters => [Id]
        , responses => Responses
        }
     },
  Path = "/pokemons/:id",
  Opts = #{ path => Path
          , model => pokemons
          , verbose => true
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].
