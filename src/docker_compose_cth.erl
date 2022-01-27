-module(docker_compose_cth).

-export([init/2,
         pre_init_per_suite/3,
         post_init_per_suite/4,
         post_init_per_group/5,
         terminate/1]).

-include_lib("common_test/include/ct.hrl").

-define(DEFAULTS, #{stop => true,
                    post_init => false,
                    docker_compose_file => undefined,
                    docker_compose_path => undefined,
                    executable_search_path => undefined,
                    check_if_running => false,
                    skip_or_fail => fail,
                    remove_volumes => false}).

-type state() :: #{stop => boolean(),
                   post_init => boolean(),
                   docker_compose_file => file:filename_all(),
                   docker_compose_path := file:filename_all() | undefined,
                   executable_search_path := [file:filename_all()],
                   check_if_running := string() | {string(), [string()]} | false,
                   skip_or_fail := skip | fail,
                   remove_volumes := boolean()}.

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
            State1 = State#{docker_compose_path => DockerCompose,
                            docker_compose_file => docker_compose_file(Config, State)},
            case up_if_not_running(State1) of
                ok ->
                    {Config, State1#{post_init => true}};
                {fail, _}=Fail ->
                    {Fail, State1#{post_init => true}}
            end
    end.

docker_compose_file(Config, #{docker_compose_file := undefined}) ->
    DataDir = ?config(data_dir, Config),
    DataDirCompose = filename:join(DataDir, "docker-compose.yml"),
    case filelib:is_file(DataDirCompose) of
        true ->
            DataDirCompose;
        false ->
            undefined
    end;
docker_compose_file(_Config, #{docker_compose_file := DockerComposeFile}) ->
    DockerComposeFile.

terminate(#{stop := false}) ->
    ok;
terminate(#{docker_compose_path := undefined}) ->
    ok;
terminate(#{docker_compose_path := DockerCompose,
            docker_compose_file := DockerComposeFile,
            remove_volumes := RemoveVolumes}) ->
    down(DockerCompose, DockerComposeFile, RemoveVolumes).

%%

-spec up_if_not_running(state()) -> ok | {fail, term()}.
up_if_not_running(#{docker_compose_path := DockerCompose,
                    check_if_running := CheckIfRunning,
                    docker_compose_file := DockerComposeFile}) ->
    case check_if_running(DockerCompose, DockerComposeFile, CheckIfRunning) of
        true ->
            ok;
        false ->
            up(DockerCompose, DockerComposeFile)
    end.

find_executable(Executable, undefined) ->
    os:find_executable(Executable);
find_executable(Executable, Path) ->
    os:find_executable(Executable, Path).

check_if_running(_DockerCompose, _DockerComposeFile, false) ->
    false;
check_if_running(DockerCompose, DockerComposeFile, {Service, Command}) ->
    0 =:= do(DockerCompose, DockerComposeFile, ["exec", "-T", Service | Command]);
check_if_running(DockerCompose, DockerComposeFile, Service) ->
    check_if_running(DockerCompose, DockerComposeFile, {Service, ["ls"]}).


up(DockerCompose, DockerComposeFile) ->
    case do(DockerCompose, DockerComposeFile, ["up", "-d"]) of
        127 ->
            {fail, "docker-compose executable not found to run"};
        0 ->
            ok;
        E ->
            {fail, io_lib:format("docker-compose failed to run, exit_status=~p", [E])}
    end.

down(DockerCompose, DockerComposeFile, RemoveVolumes) ->
    OtherArgs = case RemoveVolumes of
        true -> ["-v"];
        false -> []
    end,
    case do(DockerCompose, DockerComposeFile, ["down" | OtherArgs]) of
        0 ->
            ok;
        E ->
            ct:pal("Failed to run 'docker-compose down' exit_status=~p", [E]),
            ok
    end.

do(Executable, undefined, Args) ->
    do(Executable, Args);
do(Executable, DockerCompileFile, Args) ->
    do(Executable, ["-f", DockerCompileFile | Args]).

do(Executable, Args) ->
    Port = open_port({spawn_executable, Executable}, [exit_status,
                                                      {args, Args}]),
    receive
        {Port, {exit_status, E}} ->
            E
    end.
