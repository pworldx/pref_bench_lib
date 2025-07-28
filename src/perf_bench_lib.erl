-module(perf_bench_lib).
-behaviour(application).

-export([start/2, stop/1]).
-export([run/0, run/1, run/2, run/3]).

-define(DEFAULT_ITER, 1000000000).

%% OTP Callbacks
start(_Type, _Args) -> {ok, self()}.
stop(_State) -> ok.

%% Public APIs
run() ->
    {ok, Cores} = erlang:system_info(schedulers_online),
    run(?DEFAULT_ITER, Cores, all).

run(Workers) -> run(?DEFAULT_ITER, Workers, all).

run(Iterations, Workers) -> run(Iterations, Workers, all).

%% Mode = loop | ets | msg | all
run(Iterations, Workers, Mode) ->
    io:format("Running benchmark (~p mode) with ~p iterations and ~p workers~n",
              [Mode, Iterations, Workers]),
    case Mode of
        loop -> benchmark_loop(Iterations, Workers);
        ets  -> benchmark_ets(Iterations, Workers);
        msg  -> benchmark_msg(Iterations, Workers);
        all  -> (benchmark_loop(Iterations, Workers),
                 benchmark_ets(Iterations, Workers),
                 benchmark_msg(Iterations, Workers))
    end.

benchmark_loop(Iterations, Workers) ->
    IterPerWorker = Iterations div Workers,
    Start = erlang:monotonic_time(millisecond),
    Pids = [spawn(fun() -> perf_bench_worker:loop(IterPerWorker, self()) end)
            || _ <- lists:seq(1, Workers)],
    wait(Pids),
    End = erlang:monotonic_time(millisecond),
    io:format("✅ Loop Benchmark: ~p iterations in ~p ms (~p sec)~n",
              [Iterations, End-Start, (End-Start)/1000.0]).

benchmark_ets(Iterations, Workers) ->
    Table = ets:new(perf_bench, [set, public]),
    IterPerWorker = Iterations div Workers,
    Start = erlang:monotonic_time(millisecond),
    Pids = [spawn(fun() -> perf_bench_worker:ets_test(Table, IterPerWorker, self()) end)
            || _ <- lists:seq(1, Workers)],
    wait(Pids),
    End = erlang:monotonic_time(millisecond),
    io:format("✅ ETS Benchmark: ~p insert/read in ~p ms (~p sec)~n",
              [Iterations, End-Start, (End-Start)/1000.0]),
    ets:delete(Table).

benchmark_msg(Iterations, Workers) ->
    IterPerWorker = Iterations div Workers,
    Start = erlang:monotonic_time(millisecond),
    Pids = [spawn(fun() -> perf_bench_worker:msg_test(IterPerWorker, self()) end)
            || _ <- lists:seq(1, Workers)],
    wait(Pids),
    End = erlang:monotonic_time(millisecond),
    io:format("✅ Message Passing: ~p messages in ~p ms (~p sec)~n",
              [Iterations, End-Start, (End-Start)/1000.0]).

wait([]) -> ok;
wait([Pid | Rest]) ->
    receive
        {done, Pid, _Count} -> ok
    end,
    wait(Rest).
