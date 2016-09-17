%%% @doc GET|PATCH|DELETE /species/:name implementation
-module(poke_single_species_handler).

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
     , description => <<"species name">>
     , in => path
     , type => string
     , required => true
     },
  TypesObj =
    #{ type => <<"array">>
     , items => #{type => <<"string">>}
     , maxItems => 2
     , minItems => 1
     , uniqueItems => true
     },
  SpeciesObj =
   #{ <<"type">> => <<"object">>
    , <<"required">> => [ <<"name">>
                        , <<"types">>
                        ]
    , <<"properties">> =>
        #{ <<"name">> => #{<<"type">> => <<"string">>}
         , <<"types">> => TypesObj
         }
    },
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"fields to update">>
     , required => true
     , schema =>
         #{ <<"type">> => <<"object">>
          , <<"properties">> =>
              #{ <<"types">> => TypesObj
               }
          }
     },
  Responses =
    #{ <<"200">> =>
         #{ description => <<"successful request with response body">>
          , schema => SpeciesObj
          }
     , <<"400">> => #{description => <<"Invalid parameters">>}
     , <<"404">> => #{description => <<"Species not found">>}
     },
  Metadata =
    #{ get =>
       #{ tags => ["species"]
        , description => "Returns a species"
        , produces => ["application/json"]
        , parameters => [Id]
        , responses => Responses
        }
     , patch =>
       # { tags => ["species"]
         , description => "Updates a species"
         , consumes => ["application/json"]
         , produces => ["application/json"]
         , parameters => [RequestBody, Id]
         , responses => Responses
         }
     , delete =>
       #{ tags => ["species"]
        , description => "Deletes a species"
        , produces => ["application/json"]
        , parameters => [Id]
        , responses => Responses
        }
     },
  Path = "/species/:id",
  Opts = #{ path => Path
          , model => species
          , verbose => true
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].
