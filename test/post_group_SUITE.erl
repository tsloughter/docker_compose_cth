-module(post_group_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, docker_compose}].

groups() ->
    [{docker_compose, [check_service]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    [{ct_hooks, [docker_compose_cth]} | Config].

end_per_group(_, _Config) ->
    ok.

check_service(_Config) ->
    ok.
