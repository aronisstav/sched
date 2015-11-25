-module(sched_tests).

-compile(export_all).

%%%=============================================================================

-spec sched_proper_sample() -> true.

sched_proper_sample() ->
  sched_loader:init(),
  ok = sched_loader:load(proper_test),
  sched_simulation:enable(),
  try
    proper_test:test(),
    exit(normal)
  catch
    Class:Reason ->
      erlang:display({ok, Class, Reason, erlang:get_stacktrace()})
  end.
