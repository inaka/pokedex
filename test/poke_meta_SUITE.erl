-module(poke_meta_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ktn_meta_SUITE, [dialyzer/1, elvis/1, xref/1]}]).

-export([init_per_suite/1, all/0]).

%% @todo add xref when moving past OTP19.0.5
-spec all() -> [atom()].
all() -> [dialyzer, elvis].

-spec init_per_suite(ktn_meta_SUITE:config()) -> ktn_meta_SUITE:config().
init_per_suite(Config) -> [{application, pokenaka} | Config].
