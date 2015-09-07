%% -*- erlang-indent-level: 2 -*-

-module(sched_instrumenter).

-export([instrument/3]).

-define(inspect, sched_inspect).

-spec instrument(module(), cerl:cerl(), ets:tid()) -> cerl:cerl().

instrument(Module, CoreCode, Instrumented) ->
  true = ets:insert(Instrumented, {Module}),
  true = ets:insert(Instrumented, {{current}, Module}),
  {R, {Instrumented, _}} =
    cerl_trees:mapfold(fun mapfold/2, {Instrumented, 1}, CoreCode),
  true = ets:delete(Instrumented, {current}),
  R.

mapfold(Tree, {Instrumented, Var}) ->
  Type = cerl:type(Tree),
  NewTree =
    case Type of
      apply ->
        Op = cerl:apply_op(Tree),
        case cerl:is_c_fname(Op) of
          true -> Tree;
          false ->
            OldArgs = cerl:make_list(cerl:apply_args(Tree)),
            inspect(apply, [Op, OldArgs], Tree)
        end;
      call ->
        Module = cerl:call_module(Tree),
        Name = cerl:call_name(Tree),
        Args = cerl:call_args(Tree),
        case is_safe(Module, Name, length(Args), Instrumented) of
          true -> Tree;
          false ->
            inspect(call, [Module, Name, cerl:make_list(Args)], Tree)
        end;
      'receive' ->
        Clauses = cerl:receive_clauses(Tree),
        Timeout = cerl:receive_timeout(Tree),
        Action = cerl:receive_action(Tree),
        Fun = receive_matching_fun(Tree),
        Call = inspect('receive', [Fun, Timeout], Tree),
        case Timeout =:= cerl:c_atom(infinity) of
          false ->
            %% Replace original timeout with a fresh variable to make it
            %% skippable on demand.
            TimeoutVar = cerl:c_var(Var),
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
  NewVar =
    case Type of
      'receive' -> Var + 1;
      _ -> Var
    end,
  {NewTree, {Instrumented, NewVar}}.

inspect(Tag, Args, Tree) ->
  CTag = cerl:c_atom(Tag),
  CArgs = cerl:make_list(Args),
  cerl:update_tree(Tree, call,
                   [[cerl:c_atom(?inspect)],
                    [cerl:c_atom(inspect)],
                    [CTag, CArgs, cerl:abstract(cerl:get_ann(Tree))]]).

receive_matching_fun(Tree) ->
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
      NameLit = cerl:concrete(Name),
      ModuleLit = cerl:concrete(Module),
      %% erlang:apply/3 is safe only when called inside of erlang.erl
      case {ModuleLit, NameLit, Arity} =:= {erlang, apply, 3} of
        true -> Current =:= erlang;
        false ->
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
                ModuleLit =:= maps
                orelse
                ModuleLit =:= unicode
                orelse %% The rest are defined in concuerror.hrl
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
       {bump_reductions, 1}, %% XXX: This may change
       {dt_append_vm_tag_data, 1},
       {dt_spread_tag, 1},
       {dt_restore_tag,1},
       {error, 1},
       {error, 2},
       {exit, 1},
       {float_to_list, 1},
       {function_exported, 3},
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
