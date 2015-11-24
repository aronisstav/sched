%% -*- erlang-indent-level: 2 -*-

%% This is just an extension for a function in cerl_trees. It stays here until
%% the relevant patch is reviewed.

-module(my_cerl_trees).

-export([mapfold/4]).

-import(cerl, [alias_pat/1, alias_var/1, apply_args/1, apply_op/1,
        binary_segments/1, bitstr_val/1, bitstr_size/1, bitstr_unit/1,
        bitstr_type/1, bitstr_flags/1, call_args/1, call_module/1, call_name/1,
        case_arg/1, case_clauses/1, catch_body/1, clause_body/1, clause_guard/1,
        clause_pats/1, concrete/1, cons_hd/1, cons_tl/1, fun_body/1, fun_vars/1,
        let_arg/1, let_body/1, let_vars/1, letrec_body/1, letrec_defs/1,
        module_attrs/1, module_defs/1, module_exports/1, module_name/1,
        primop_args/1, primop_name/1, receive_action/1, receive_clauses/1,
        receive_timeout/1, seq_arg/1, seq_body/1, try_arg/1, try_body/1,
        try_vars/1, try_evars/1, try_handler/1, tuple_es/1, type/1,
        update_c_alias/3, update_c_apply/3, update_c_binary/2,
        update_c_bitstr/6, update_c_call/4, update_c_case/3, update_c_catch/2,
        update_c_clause/4, update_c_cons/3, update_c_cons_skel/3,
        update_c_fun/3, update_c_let/4, update_c_letrec/3, update_c_module/5,
        update_c_primop/3, update_c_receive/4, update_c_seq/3, update_c_try/6,
        update_c_tuple/2, update_c_tuple_skel/2, update_c_values/2, values_es/1,
        map_arg/1, map_es/1, update_c_map_pair/4, update_c_map/3,
        map_pair_key/1,map_pair_val/1,map_pair_op/1]).

%% @spec mapfold(Pre, Post, Initial::term(), Tree::cerl()) ->
%%           {cerl(), term()}
%%
%%    Pre = Post = (cerl(), term()) -> {cerl(), term()}
%%
%% @doc Does a combined map/fold operation on the nodes of the
%% tree. It begins by calling <code>Pre</code> on the tree, using the
%% <code>Initial</code> value. It then deconstructs the top node of
%% the returned tree and recurses on the children, using the returned
%% value as the new initial and carrying the returned values from one
%% call to the next. Finally it reassembles the top node from the
%% children, calls <code>Post</code> on it and returns the result.

-spec mapfold(fun((cerl:cerl(), term()) -> {cerl:cerl(), term()}),
              fun((cerl:cerl(), term()) -> {cerl:cerl(), term()}),
	      term(), cerl:cerl()) -> {cerl:cerl(), term()}.

mapfold(Pre, Post, S00, T0) ->
  {T, S0} = Pre(T0, S00),
  case type(T) of
    literal ->
      case concrete(T) of
        [_ | _] ->
          {T1, S1} = mapfold(Pre, Post, S0, cons_hd(T)),
          {T2, S2} = mapfold(Pre, Post, S1, cons_tl(T)),
          Post(update_c_cons(T, T1, T2), S2);
        V when tuple_size(V) > 0 ->
          {Ts, S1} = mapfold_list(Pre, Post, S0, tuple_es(T)),
          Post(update_c_tuple(T, Ts), S1);
        _ ->
          Post(T, S0)
      end;
    var ->
      Post(T, S0);
    values ->
      {Ts, S1} = mapfold_list(Pre, Post, S0, values_es(T)),
      Post(update_c_values(T, Ts), S1);
    cons ->
      {T1, S1} = mapfold(Pre, Post, S0, cons_hd(T)),
      {T2, S2} = mapfold(Pre, Post, S1, cons_tl(T)),
      Post(update_c_cons_skel(T, T1, T2), S2);
    tuple ->
      {Ts, S1} = mapfold_list(Pre, Post, S0, tuple_es(T)),
      Post(update_c_tuple_skel(T, Ts), S1);
    map ->
      {M , S1} = mapfold(Pre, Post, S0, map_arg(T)),
      {Ts, S2} = mapfold_list(Pre, Post, S1, map_es(T)),
      Post(update_c_map(T, M, Ts), S2);
    map_pair ->
      {Op,  S1} = mapfold(Pre, Post, S0, map_pair_op(T)),
      {Key, S2} = mapfold(Pre, Post, S1, map_pair_key(T)),
      {Val, S3} = mapfold(Pre, Post, S2, map_pair_val(T)),
      Post(update_c_map_pair(T,Op,Key,Val), S3);
    'let' ->
      {Vs, S1} = mapfold_list(Pre, Post, S0, let_vars(T)),
      {A, S2} = mapfold(Pre, Post, S1, let_arg(T)),
      {B, S3} = mapfold(Pre, Post, S2, let_body(T)),
      Post(update_c_let(T, Vs, A, B), S3);
    seq ->
      {A, S1} = mapfold(Pre, Post, S0, seq_arg(T)),
      {B, S2} = mapfold(Pre, Post, S1, seq_body(T)),
      Post(update_c_seq(T, A, B), S2);
    apply ->
      {E, S1} = mapfold(Pre, Post, S0, apply_op(T)),
      {As, S2} = mapfold_list(Pre, Post, S1, apply_args(T)),
      Post(update_c_apply(T, E, As), S2);
    call ->
      {M, S1} = mapfold(Pre, Post, S0, call_module(T)),
      {N, S2} = mapfold(Pre, Post, S1, call_name(T)),
      {As, S3} = mapfold_list(Pre, Post, S2, call_args(T)),
      Post(update_c_call(T, M, N, As), S3);
    primop ->
      {N, S1} = mapfold(Pre, Post, S0, primop_name(T)),
      {As, S2} = mapfold_list(Pre, Post, S1, primop_args(T)),
      Post(update_c_primop(T, N, As), S2);
    'case' ->
      {A, S1} = mapfold(Pre, Post, S0, case_arg(T)),
      {Cs, S2} = mapfold_list(Pre, Post, S1, case_clauses(T)),
      Post(update_c_case(T, A, Cs), S2);
    clause ->
      {Ps, S1} = mapfold_list(Pre, Post, S0, clause_pats(T)),
      {G, S2} = mapfold(Pre, Post, S1, clause_guard(T)),
      {B, S3} = mapfold(Pre, Post, S2, clause_body(T)),
      Post(update_c_clause(T, Ps, G, B), S3);
    alias ->
      {V, S1} = mapfold(Pre, Post, S0, alias_var(T)),
      {P, S2} = mapfold(Pre, Post, S1, alias_pat(T)),
      Post(update_c_alias(T, V, P), S2);
    'fun' ->
      {Vs, S1} = mapfold_list(Pre, Post, S0, fun_vars(T)),
      {B, S2} = mapfold(Pre, Post, S1, fun_body(T)),
      Post(update_c_fun(T, Vs, B), S2);
    'receive' ->
      {Cs, S1} = mapfold_list(Pre, Post, S0, receive_clauses(T)),
      {E, S2} = mapfold(Pre, Post, S1, receive_timeout(T)),
      {A, S3} = mapfold(Pre, Post, S2, receive_action(T)),
      Post(update_c_receive(T, Cs, E, A), S3);
    'try' ->
      {E, S1} = mapfold(Pre, Post, S0, try_arg(T)),
      {Vs, S2} = mapfold_list(Pre, Post, S1, try_vars(T)),
      {B, S3} = mapfold(Pre, Post, S2, try_body(T)),
      {Evs, S4} = mapfold_list(Pre, Post, S3, try_evars(T)),
      {H, S5} = mapfold(Pre, Post, S4, try_handler(T)),
      Post(update_c_try(T, E, Vs, B, Evs, H), S5);
    'catch' ->
      {B, S1} = mapfold(Pre, Post, S0, catch_body(T)),
      Post(update_c_catch(T, B), S1);
    binary ->
      {Ds, S1} = mapfold_list(Pre, Post, S0, binary_segments(T)),
      Post(update_c_binary(T, Ds), S1);
    bitstr ->
      {Val, S1} = mapfold(Pre, Post, S0, bitstr_val(T)),
      {Size, S2} = mapfold(Pre, Post, S1, bitstr_size(T)),
      {Unit, S3} = mapfold(Pre, Post, S2, bitstr_unit(T)),
      {Type, S4} = mapfold(Pre, Post, S3, bitstr_type(T)),
      {Flags, S5} = mapfold(Pre, Post, S4, bitstr_flags(T)),
      Post(update_c_bitstr(T, Val, Size, Unit, Type, Flags), S5);
    letrec ->
      {Ds, S1} = mapfold_pairs(Pre, Post, S0, letrec_defs(T)),
      {B, S2} = mapfold(Pre, Post, S1, letrec_body(T)),
      Post(update_c_letrec(T, Ds, B), S2);
    module ->
      {N, S1} = mapfold(Pre, Post, S0, module_name(T)),
      {Es, S2} = mapfold_list(Pre, Post, S1, module_exports(T)),
      {As, S3} = mapfold_pairs(Pre, Post, S2, module_attrs(T)),
      {Ds, S4} = mapfold_pairs(Pre, Post, S3, module_defs(T)),
      Post(update_c_module(T, N, Es, As, Ds), S4)
  end.

mapfold_list(Pre, Post, S0, [T | Ts]) ->
  {T1, S1} = mapfold(Pre, Post, S0, T),
  {Ts1, S2} = mapfold_list(Pre, Post, S1, Ts),
  {[T1 | Ts1], S2};
mapfold_list(_, _, S, []) ->
  {[], S}.

mapfold_pairs(Pre, Post, S0, [{T1, T2} | Ps]) ->
  {T3, S1} = mapfold(Pre, Post, S0, T1),
  {T4, S2} = mapfold(Pre, Post, S1, T2),
  {Ps1, S3} = mapfold_pairs(Pre, Post, S2, Ps),
  {[{T3, T4} | Ps1], S3};
mapfold_pairs(_, _, S, []) ->
  {[], S}.
