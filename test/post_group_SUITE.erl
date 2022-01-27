-module(post_group_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, docker_compose_multirun}].

groups() ->
    [
        {docker_compose, [check_create_table]},
        {docker_compose_multirun, [{repeat, 2}], [{group, docker_compose}]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(docker_compose, Config) ->
    [{ct_hooks, [{docker_compose_cth, #{remove_volumes => true}}]} | Config];
init_per_group(_, Config) -> Config.

end_per_group(_, _Config) ->
    ok.

check_create_table(_Config) ->
    timer:sleep(2000),
    {ok, C} = epgsql:connect(#{
        host => "localhost",
        username => "cth",
        password => "cth",
        database => "cth",
        timeout => 4000
    }),
    {ok, [], []} = epgsql:squery(C, "CREATE TABLE test()"),
    ok = epgsql:close(C).
