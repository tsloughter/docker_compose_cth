Common Test Docker Compose Hook
=====

A [Common Test](http://erlang.org/doc/apps/common_test/) hook for starting and stopping [docker-compose](https://docs.docker.com/compose/) services.

Hooks can be declared in `rebar.config` with `ct_opts`:

``` erlang
{ct_opts, [{ct_hooks, [docker_compose_cth]}]}.
```

It can be added on a per-suite either with the `suite/0` function in the suite module:

``` erlang
suite() ->
    [{ct_hooks, [docker_compose_cth]}].
```

or in the return of `init_per_suite/1`:

``` erlang
init_per_suite(Config) ->
    [{ct_hooks, [docker_compose_cth]} | Config].
```

For groups it can be added per-group in the return of `init_per_group/2` function in the suite module:

``` erlang
init_per_group(Group, Config) ->
    [{ct_hooks, [docker_compose_cth]} | Config].
```

Shutdown of the services happens in the hooks `terminate` function which is called depending on when the hook was installed: [CTH Scope](http://erlang.org/doc/apps/common_test/ct_hooks_chapter.html#cth-scope). But the configuration for the hook can include `#{stop => never}` and the hook will not shutdown the services during `terminate`.

## Configuration

* `executable_search_paths`: Path to search for the `docker-compose` executable. If it is not set the regular path is searched as described in the [docs for os:find_executable/1](http://erlang.org/doc/man/os.html#find_executable-1).
* `check_if_running`: A string that is the name of the service to check for before running `docker-compose up`. If defined the hook will check if a service is already up and not call `docker-compose up` if it is. By default this is done by attempting `exec ls` in the service's container, but the command can be set to anything by passing a tuple `{Service, Command}`
* `skip_or_fail`: The atom `skip` or `fail` (default is `fail`). If the hook is unable to bring up the service with docker-compose then the suite will either skip or fail, depending on this configuration.
* `stop`: Boolean value that if `false` means the services with not be shutdown when the hook terminates.

## Todo

* Support a path to the `docker-compose.yml` file.

