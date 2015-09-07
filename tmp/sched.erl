-module(sched).

%%%=============================================================================

-export([run/1, run/2, replay/2, replay/3, trace_to_string/2]).

%%%=============================================================================

-export_type([fun_or_funs/0, fun_to_sched/0, options/0, trace/0]).

%%%=============================================================================
%%% EXPORTED TYPES
%%%=============================================================================

-type fun_or_funs()  :: fun_to_sched() | [fun_to_sched()].

-type fun_to_sched() :: fun(() -> term()).

%%%-----------------------------------------------------------------------------

-type options() :: sched_options:options().

%%%-----------------------------------------------------------------------------

-type trace() :: [sched_event:event()].

%%%=============================================================================
%%% EXPORTED FUNCTIONS
%%%=============================================================================

-spec run(fun_or_funs()) -> trace().

run(Functions) ->
  run(Functions, []).

-spec run(fun_or_funs(), options()) -> trace().

run(Functions, Options) ->
  replay(Functions, [], Options).

%%%=============================================================================

-spec replay(fun_or_funs(), trace()) -> trace().

replay(Functions, Replay) ->
  replay(Functions, Replay, []).

-spec replay(fun_or_funs(), trace(), options()) -> trace().

replay([], _Replay, _Options) ->
  error(no_functions);
replay(Functions, Replay, Options) when is_list(Functions) ->
  _ = application:start(sched),
  Pred = fun(F) -> {arity, 0} =:= erlang:fun_info(F, arity) end,
  try
    true = lists:all(Pred, Functions)
  catch
    _:_ -> error(bad_function)
  end,
  Processes = sched_server:start_processes(Functions, Options),
  sched_scheduler:schedule(Processes, Replay, Options);
replay(Functions, Replay, Options) ->
  replay([Functions], Replay, Options).

%%%=============================================================================

-spec trace_to_string(trace()) -> string().

trace_to_string(Trace) ->
  string:join([sched_event:to_string(E) <- Trace], "\n").
