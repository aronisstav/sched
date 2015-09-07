%% -*- erlang-indent-level: 2 -*-

%% This module will never be instrumented.

%%%=============================================================================

-module(sched_wrapper).

-export([enable_inspect/0, inspect/3]).

%%%=============================================================================

-export_type([tag/0]).

-type tag() :: 'apply' | 'call' | 'receive'.

%%%=============================================================================

-spec enable_inspect() -> ok.

enable_inspect() ->
  put(sched, ok),
  ok.

%%%-----------------------------------------------------------------------------

-spec inspect(tag(), [term()], term()) -> term(). %XXX Refine location?

inspect(Tag, Args, Location) ->
  Action =
    case erase(sched) =:= ok of
      true ->
        put(sched, ok),
        decide(Tag, Args);
      false -> doit
    end,
  case Action of
    doit ->
      case {Tag, Args} of
        {apply, [Fun, ApplyArgs]} ->
          erlang:apply(Fun, ApplyArgs);
        {call, [Module, Name, CallArgs]} ->
          erlang:apply(Module, Name, CallArgs);
        {'receive', [_, Timeout]} ->
          Timeout
      end;
    {call, NewArgs} ->
      inspect(call, NewArgs, Location)
  end.

%%%=============================================================================

decide(apply, [Fun, ApplyArgs]) ->
  case is_function(Fun) of
    true ->
      Module = get_fun_info(Fun, module),
      Name = get_fun_info(Fun, name),
      Arity = get_fun_info(Fun, arity),
      case erlang:is_builtin(Module, Name, Arity) of
        true -> {call, [Module, Name, ApplyArgs]};
        false -> doit
      end;
    false -> doit
  end;
decide(call, [Module, Name, CallArgs]) ->
  case erlang:is_builtin(Module, Name, length(CallArgs)) of
    true ->
      erlang:display({wrapped, Module, Name, length(CallArgs)}),
      doit;
    false ->
      _ = sched_loader:load(Module),
      doit
  end;
decide(_, _) ->
  doit.

get_fun_info(Fun, Tag) ->
  {Tag, Info} = erlang:fun_info(Fun, Tag),
  Info.
