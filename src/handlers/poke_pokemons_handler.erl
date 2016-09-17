%%% @doc GET|POST /pokemons implementation
-module(poke_pokemons_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , handle_post/2
          ]
        }]).

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
  Path = "/pokemons",
  Opts = #{ path => Path
          , model => pokemons
          , verbose => true
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].
