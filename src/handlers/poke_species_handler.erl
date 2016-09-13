-module(poke_species_handler).

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
  SpeciesObj =
   #{ <<"type">> => <<"object">>
    , <<"required">> => [ <<"name">>
                        , <<"types">>
                        ]
    , <<"properties">> =>
        #{ <<"name">> => #{<<"type">> => <<"string">>}
         , <<"types">> =>
            #{ type => <<"array">>
             , items => #{type => <<"string">>}
             , maxItems => 2
             , minItems => 1
             , uniqueItems => true
             }
         }
    },
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"a species">>
     , required => true
     , schema => SpeciesObj
     },
  PostResponses =
    #{ <<"201">> =>
         #{ description => <<"successful request with response body">>
          , schema => SpeciesObj
          }
     , <<"400">> => #{description => <<"Invalid parameters">>}
     },
  GetResponses =
    #{ <<"200">> =>
         #{ description => <<"a list of species">>
          , schema =>
              #{ type => <<"array">>
               , items => SpeciesObj
               }
          }
     },
  Metadata =
    #{ get =>
       #{ tags => ["species"]
        , description => "Returns the list of species"
        , produces => ["application/json"]
        , responses => GetResponses
        }
     , post =>
       # { tags => ["species"]
         , description => "Creates a new species"
         , consumes => ["application/json"]
         , produces => ["application/json"]
         , parameters => [RequestBody]
         , responses => PostResponses
         }
     },
  Path = "/species",
  Opts = #{ path => Path
          , model => species
          , verbose => true
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].
