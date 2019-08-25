-module(docker_compose_cth).

-export([init/2,
         pre_init_per_suite/3,
         post_init_per_suite/4,
         post_init_per_group/5,
         terminate/1]).

-define(DEFAULTS, #{stop => true,
                    post_init => false,
                    docker_compose_path => undefined,
                    executable_search_path => undefined,
                    check_if_running => false,
                    skip_or_fail => fail}).

-type state() :: #{stop => boolean(),
                   post_init => boolean(),
                   docker_compose_path := filename:filename_all() | undefined,
                   executable_search_path := [filename:filename_all()],
                   check_if_running := string() | {string(), [string()]} | false,
                   skip_or_fail := skip | fail}.

%% Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, Opts) when is_map(Opts) ->
    State = maps:merge(?DEFAULTS, ct:get_config(docker_compose_cth, #{})),
    {ok, maps:merge(State, Opts)};
init(_Id, _Opts) ->
    State = maps:merge(?DEFAULTS, ct:get_config(docker_compose_cth, #{})),
    {ok, State}.

%% Called before init_per_suite is called.
pre_init_per_suite(_Suite, Config, State) ->
    do_init(Config, State).

%% Called after init_per_suite is called.
post_init_per_suite(_Suite, _Config, Return, State=#{post_init := false}) ->
    do_init(Return, State);
post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

%% Called after init_per_group is called.
post_init_per_group(_Suite, _Group, _Config, Return, State=#{post_init := false}) ->
    do_init(Return, State);
post_init_per_group(_Suite, _Group, _Config, Return, State) ->
    {Return, State}.

do_init(Config, State=#{executable_search_path := Path}) ->
    case find_executable("docker-compose", Path) of
        false ->
            {{fail, "docker-compose not found in PATH"}, State#{post_init => true}};
        DockerCompose ->
            State1 = State#{docker_compose_path => DockerCompose},
            case up_if_not_running(State1) of
                ok ->
                    {Config, State1#{post_init => true}};
                {fail, _}=Fail ->
                    {Fail, State1#{post_init => true}}
            end
    end.

terminate(#{stop := false}) ->
    ok;
terminate(#{docker_compose_path := undefined}) ->
    ok;
terminate(#{docker_compose_path := DockerCompose}) ->
    down(DockerCompose).

%%

-spec up_if_not_running(state()) -> ok | {fail, term()}.
up_if_not_running(#{docker_compose_path := DockerCompose,
                    check_if_running := CheckIfRunning}) ->
    case check_if_running(DockerCompose, CheckIfRunning) of
        true ->
            ok;
        false ->
            up(DockerCompose)
    end.


find_executable(Executable, undefined) ->
    os:find_executable(Executable);
find_executable(Executable, Path) ->
    os:find_executable(Executable, Path).

check_if_running(_DockerCompose, false) ->
    false;
check_if_running(DockerCompose, {Service, Command}) ->
    0 =:= do(DockerCompose, ["exec", "-T", Service | Command]);
check_if_running(DockerCompose, Service) ->
    check_if_running(DockerCompose, {Service, ["ls"]}).


up(DockerCompose) ->
    case do(DockerCompose, ["up", "-d"]) of
        127 ->
            {fail, "docker-compose executable not found to run"};
        0 ->
            ok;
        E ->
            {fail, io_lib:format("docker-compose failed to run, exit_status=~p", [E])}
    end.

down(DockerCompose) ->
    case do(DockerCompose, ["down"]) of
        0 ->
            ok;
        E ->
            ct:pal("Failed to run 'docker-compose down' exit_status=~p", [E]),
            ok
    end.

do(Executable, Args) ->
    Port = open_port({spawn_executable, Executable}, [exit_status,
                                                      {args, Args}]),
    receive
        {Port, {exit_status, E}} ->
            E
    end.
