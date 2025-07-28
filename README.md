Erlang Performance Benchmark Library

A lightweight Erlang library to benchmark system performance using:
- 🔹 CPU-bound loops (up to 1 billion iterations)
- 🔹 ETS (Erlang Term Storage) insert/read performance
- 🔹 Message passing overhead between processes

---

##  Features
Uses Erlang's concurrency model with lightweight processes  
Auto-detects CPU cores for optimal parallelism  
Benchmarks raw loops, ETS, and message passing  
Configurable iterations and workers  
Reusable as a library or standalone tool  

Add this to your erlang project:

{deps, [
  {perf_bench_lib, {git, "https://github.com/YOUR-USERNAME/perf_bench_lib.git", {branch, "main"}}}
]}.

Tests :
1> perf_bench_lib:run().
2> perf_bench_lib:run(500000000, 4).  % 500M iterations, 4 workers
3> perf_bench_lib:run(1000000, 4, ets).   % ETS test
4> perf_bench_lib:run(5000000, 2, msg).   % Message passing test
5> perf_bench_lib:run(200000000, 8, loop).% Raw loop test

Results :
Running benchmark (all mode) with 1000000000 iterations and 8 workers
Loop Benchmark: 1000000000 iterations in 1420 ms (1.42 sec)
ETS Benchmark: 1000000000 insert/read in 2840 ms (2.84 sec)
Message Passing: 1000000000 messages in 3650 ms (3.65 sec)
