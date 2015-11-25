%% -*- erlang-indent-level: 2 -*-

%%%=============================================================================

-module(sched_loader).

-export([init/0, load/1, error_to_string/1]).

-export_type([location/0, receive_pattern_fun()]).

-include("sched.hrl").

%%%=============================================================================

-type ascii_string()        :: [1..255,...].
-type error_reason()        :: 'cannot_instrument_wrapper'.
-opaque location()          :: term(). %XXX: Refine?
-type module_atom()         :: atom().
-type receive_pattern_fun() :: fun((term()) -> boolean()).

%%%=============================================================================

-define(wrapper, sched_wrapper).
-define(instrumented_tag, sched_compiled).

-define(ERROR(A), {'error', {?MODULE, A}}).

%%%=============================================================================

-spec init() -> ok.

init() ->
  {module, ?wrapper} = code:load_file(?wrapper),
  _ = [true = code:unstick_mod(M) || {M, preloaded} <- code:all_loaded()],
  [] = [D || D <- code:get_path(), ok =/= code:unstick_dir(D)],
  case code:get_object_code(erlang) =:= error of
    true ->
      true =
        code:add_pathz(filename:join(code:root_dir(), "erts/preloaded/ebin")),
      ok;
    false ->
      ok
  end.

%%%-----------------------------------------------------------------------------

-spec load(module_atom()) -> 'ok' | ?ERROR(error_reason()).

load(?wrapper) -> ?ERROR(cannot_instrument_wrapper);
load(Module) ->
  Which = confuse(which(Module)),
  case Which =:= ?instrumented_tag of
    true -> ok;
    false ->
      ?d({load, Module}),
      Beam =
        case Which =:= preloaded of
          true ->
            {Module, BeamBinary, _} = code:get_object_code(Module),
            BeamBinary;
          false -> Which
        end,
      load_binary(Module, Beam)
  end.

%%%-----------------------------------------------------------------------------

-spec error_to_string(error_reason()) -> ascii_string().

error_to_string(cannot_instrument_wrapper) ->
  "Cannot instrument wrapper!".

%%%=============================================================================

which(Foo) ->
  code:which(Foo).

%% The spec of 'code' functions does not let for custom tags, so Dialyzer
%% complains. This will throw him off our tail.
confuse(Foo) ->
  orddict:fetch(confuse_dialyzer, orddict:from_list([{confuse_dialyzer, Foo}])).

load_binary(Module, Beam) ->
  Core = get_core(Beam),
  InstrumentedCore = instrument(Module, Core),
  {ok, _, NewBinary} =
    compile:forms(InstrumentedCore, [from_core, report_errors, binary]),
  Tag = confuse(?instrumented_tag),
  {module, Module} = code:load_binary(Module, Tag, NewBinary),
  ok.

get_core(Beam) ->
  {ok, {Module, [{abstract_code, ChunkInfo}]}} =
    beam_lib:chunks(Beam, [abstract_code]),
  case ChunkInfo of
    {_, Chunk} ->
      {ok, Module, Core} = compile:forms(Chunk, [binary, to_core0]),
      Core;
    no_abstract_code ->
      {ok, {Module, [{compile_info, CompileInfo}]}} =
        beam_lib:chunks(Beam, [compile_info]),
      {source, File} = proplists:lookup(source, CompileInfo),
      {options, CompileOptions} = proplists:lookup(options, CompileInfo),
      Filter =
        fun(Option) ->
            case Option of
              {Tag, _} -> lists:member(Tag, [d, i]);
              _ -> false
            end
        end,
      CleanOptions = lists:filter(Filter, CompileOptions),
      Options = [debug_info, report_errors, binary, to_core0|CleanOptions],
      {ok, Module, Core} = compile:file(File, Options),
      Core
  end.

%%-spec instrument(module(), cerl:cerl()) -> cerl:cerl().

instrument(Current, CoreCode) ->
  {R, {Current, _}} =
    my_cerl_trees:mapfold(fun pre/2, fun post/2, {Current, {[], 1}}, CoreCode),
  %% io:format("~p~n",[R]),
  R.

pre(Tree, {Current, {SelfNs, TimeoutN} = Ns}) ->
  Type = cerl:type(Tree),
  NewNs =
    case Type of
      'fun' ->
        {[TimeoutN|SelfNs], TimeoutN + 1};
      _ ->
        Ns
    end,
  {Tree, {Current, NewNs}}.

post(Tree, {Current, {SelfNs, TimeoutN} = Ns}) ->
  Type = cerl:type(Tree),
  NewTree =
    case Type of
      'apply' ->
        Op = cerl:apply_op(Tree),
        case cerl:is_c_fname(Op) of
          true -> Tree;
          false ->
            OldArgs = cerl:make_list(cerl:apply_args(Tree)),
            inspect(apply, [Op, OldArgs], Tree)
        end;
      'call' ->
        Module = cerl:call_module(Tree),
        Name = cerl:call_name(Tree),
        Args = cerl:call_args(Tree),
        case is_safe(Module, Name, length(Args), Current) of
          is_self ->
            [SelfN|_] = SelfNs,
            cerl:c_var(SelfN);
          true ->
            Tree;
          false ->
            inspect(call, [Module, Name, cerl:make_list(Args)], Tree)
        end;
      'fun' ->
        Vars = cerl:fun_vars(Tree),
        Body = cerl:fun_body(Tree),
        [SelfN|_] = SelfNs,
        SelfVar = cerl:c_var(SelfN),
        SelfCall =
          inspect(call, [cerl:abstract(Y) || Y <- [erlang, self, []]], Tree),
        WithSelfVar =
          cerl:update_tree(Tree, 'let', [[SelfVar], [SelfCall], [Body]]),
        cerl:update_c_fun(Tree, Vars, WithSelfVar);
      'receive' ->
        Clauses = cerl:receive_clauses(Tree),
        Timeout = cerl:receive_timeout(Tree),
        Action = cerl:receive_action(Tree),
        Fun = receive_pattern_fun(Tree),
        Call = inspect('receive', [Fun, Timeout], Tree),
        case Timeout =:= cerl:c_atom(infinity) of
          false ->
            %% Replace original timeout with a fresh variable to make it
            %% skippable on demand.
            TimeoutVar = cerl:c_var(TimeoutN),
            RecTree = cerl:update_c_receive(Tree, Clauses, TimeoutVar, Action),
            cerl:update_tree(Tree, 'let', [[TimeoutVar], [Call], [RecTree]]);
          true ->
            %% Leave infinity timeouts unaffected, as the default code generated
            %% by the compiler does not bind any additional variables in the
            %% after clause.
            cerl:update_tree(Tree, seq, [[Call], [Tree]])
        end;
      _ -> Tree
    end,
  NewNs =
    case Type of
      'receive' -> {SelfNs, TimeoutN + 1};
      'fun' ->
        [_|Rest] = SelfNs,
        {Rest, TimeoutN};
      _ -> Ns
    end,
  {NewTree, {Current, NewNs}}.

inspect(Tag, Args, Tree) ->
  CTag = cerl:c_atom(Tag),
  CArgs = cerl:make_list(Args),
  cerl:update_tree(Tree, call,
                   [[cerl:c_atom(?wrapper)],
                    [cerl:c_atom(inspect)],
                    [CTag, CArgs, cerl:abstract(cerl:get_ann(Tree))]]).

receive_pattern_fun(Tree) ->
  Msg = cerl:c_var(message),
  Clauses = extract_patterns(cerl:receive_clauses(Tree)),
  Body = cerl:update_tree(Tree, 'case', [[Msg], Clauses]),
  cerl:update_tree(Tree, 'fun', [[Msg], [Body]]).

extract_patterns(Clauses) ->
  extract_patterns(Clauses, []).

extract_patterns([], Acc) ->
  Pat = [cerl:c_var(message)],
  Guard = cerl:c_atom(true),
  Body = cerl:c_atom(false),
  lists:reverse([cerl:c_clause(Pat, Guard, Body)|Acc]);
extract_patterns([Tree|Rest], Acc) ->
  Body = cerl:c_atom(true),
  Pats = cerl:clause_pats(Tree),
  Guard = cerl:clause_guard(Tree),
  extract_patterns(Rest, [cerl:update_c_clause(Tree, Pats, Guard, Body)|Acc]).

is_safe(Module, Name, Arity, Current) ->
  case
    cerl:is_literal(Module) andalso
    cerl:is_literal(Name)
  of
    false -> false;
    true ->
      ModuleLit = cerl:concrete(Module),
      NameLit = cerl:concrete(Name),
      case {ModuleLit, NameLit, Arity} of
        %% erlang:apply/3 is safe only when called inside of erlang.erl
        {erlang, apply, 3} -> Current =:= erlang;
        {erlang, self, 0} -> is_self;
        _ ->
          case erlang:is_builtin(ModuleLit, NameLit, Arity) of
            true ->
              (ModuleLit =:= erlang
               andalso
                 (erl_internal:guard_bif(NameLit, Arity)
                  orelse erl_internal:arith_op(NameLit, Arity)
                  orelse erl_internal:bool_op(NameLit, Arity)
                  orelse erl_internal:comp_op(NameLit, Arity)
                  orelse erl_internal:list_op(NameLit, Arity)
                 )
              ) orelse
                ModuleLit =:= binary
                orelse
                ModuleLit =:= math
                orelse
                ModuleLit =:= maps
                orelse
                ModuleLit =:= unicode
                orelse
                lists:member({ModuleLit, NameLit, Arity}, race_free_bifs());
            false ->
              Current =:= ModuleLit
          end
      end
  end.

race_free_bifs() ->

  [{erlang, N, A} ||
    {N, A} <-
      [
       {atom_to_list,1},
       {'bor', 2},
       {binary_to_list, 1},
       {binary_to_term, 1},
       {get_module_info, 2},
       {bump_reductions, 1}, %% XXX: This may change
       {dt_append_vm_tag_data, 1},
       {dt_spread_tag, 1},
       {dt_restore_tag,1},
       {erase, 0},
       {erase, 1},
       {error, 1},
       {error, 2},
       {exit, 1},
       {float_to_list, 1},
       {function_exported, 3},
       {get, 1},
       {integer_to_list,1},
       {iolist_size, 1},
       {iolist_to_binary, 1},
       {list_to_atom, 1},
       {list_to_binary, 1},
       {list_to_integer, 1},
       {list_to_tuple, 1},
       {make_fun, 3},
       {make_tuple, 2},
       {md5, 1},
       {phash, 2},
       {phash2, 1},
       {put, 2},
       {raise, 3},
       {ref_to_list,1},
       {setelement, 3},
       {term_to_binary, 1},
       {throw, 1},
       {tuple_to_list, 1}
      ]]

    ++ [{error_logger, N, A} ||
         {N, A} <-
           [
            {warning_map, 0}
           ]]

    ++ [{file, N, A} ||
         {N, A} <-
           [
            {native_name_encoding, 0}
           ]]

    ++ [{lists, N, A} ||
         {N, A} <-
           [
            {keyfind, 3},
            {keymember, 3},
            {keysearch, 3},
            {keystore, 4},
            {member, 2},
            {reverse, 2}
           ]]

    ++ [{net_kernel, N, A} ||
         {N, A} <-
           [
            {dflag_unicode_io, 1}
           ]]

    ++ [{prim_file, N, A} ||
         {N, A} <-
           [
            {internal_name2native, 1}
           ]].
