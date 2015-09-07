-module(sched_tests).

-compile(export_all).

%%%=============================================================================

-spec sched_proper_sample() -> ok.

sched_proper_sample() ->
  sched_loader:init(),
  ok = sched_loader:load(proper_gen),
  ok = sched_loader:load(proper_types),
  sched_wrapper:enable_inspect(),
  proper_gen:pick(proper_types:integer()),
  ok.
