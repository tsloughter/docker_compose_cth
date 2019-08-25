-module(docker_compose_cth_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [check_service].

suite() ->
    [{ct_hooks, [docker_compose_cth]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

check_service(_Config) ->
    ok.
