-module(perf_bench_worker).
-export([loop/2, ets_test/3, msg_test/2]).

loop(0, Parent) -> Parent ! {done, self(), 0};
loop(N, Parent) -> loop(N-1, Parent).

ets_test(_Table, 0, Parent) ->
    Parent ! {done, self(), 0};
ets_test(Table, N, Parent) ->
    ets:insert(Table, {N, N}),
    _ = ets:lookup(Table, N),
    ets_test(Table, N-1, Parent).

msg_test(0, Parent) ->
    Parent ! {done, self(), 0};
msg_test(N, Parent) ->
    self() ! N,
    receive _ -> ok end,
    msg_test(N-1, Parent).
