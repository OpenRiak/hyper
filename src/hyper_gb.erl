-module(hyper_gb).

-behaviour(hyper_register).
-export([new/1,
         set/3,
         max_merge/1,
         max_merge/2,
         reduce_precision/2,
         bytes/1,
         register_sum/1,
         zero_count/1,
         encode_registers/1,
         decode_registers/2,
         compact/1]).

-type hyper_tree() :: {gb_trees:tree(integer(), number()), number()}.


-spec new(number()) -> hyper_tree().
new(P) ->
    {gb_trees:empty(), trunc(math:pow(2, P))}.

-spec get(integer(), hyper_tree()) -> {ok, number()} | undefined.
get(Index, {T, _M}) ->
    case gb_trees:lookup(Index, T) of
        {value, V} ->
            {ok, V};
        none ->
            undefined
    end.

-spec set(integer(), number(), hyper_tree()) -> hyper_tree().
set(Index, Value, {T, M}) ->
    case gb_trees:lookup(Index, T) of
        {value, R} when R > Value->
            {T, M};
        _ ->
            {gb_trees:enter(Index, Value, T), M}
    end.

-spec max_merge(list(hyper_tree())) -> hyper_tree().
max_merge(Registers) ->
    [First | Rest] = Registers,
    lists:foldl(fun (R, Acc) -> max_merge(R, Acc) end, First, Rest).

-spec max_merge(hyper_tree(), hyper_tree()) -> hyper_tree().
max_merge({Tree, _M}, InitAccTree) ->
    TreeIter = gb_trees:iterator(Tree),
    max_merge_mergetree(TreeIter, InitAccTree).

max_merge_mergetree(TreeIter, AccTree) ->
    case gb_trees:next(TreeIter) of
        none ->
            AccTree;
        {Index, Value, TreeIter2} ->
            case get(Index, AccTree) of
                {ok, R} when R >= Value ->
                    max_merge_mergetree(TreeIter2, AccTree);
                _ ->
                    max_merge_mergetree(TreeIter2, set(Index, Value, AccTree))
            end
    end.


reduce_precision(_NewP, _Register) ->
    throw(not_implemented).


bytes({T, _}) ->
    erts_debug:flat_size(T) * 8.


-spec tree_sum(gb_trees:iter(integer(), number()),{integer(), number()} ) -> {integer(), number()}.
tree_sum(TreeIter, {MaxIndex, Sum}) ->
    case gb_trees:next(TreeIter) of
        none ->
            {MaxIndex, Sum};
        {Index, Value, TreeIter2} ->
            Zeroes = Index - MaxIndex - 1,
            tree_sum(TreeIter2,
                        {Index, Sum + math:pow(2, -Value) + float(Zeroes)})
    end.

-spec register_sum(hyper_tree()) -> float().
register_sum({T, M}) ->
    TreeIter = gb_trees:iterator(T),
    {MaxI, Sum} = tree_sum(TreeIter, {-1, 0}),
    Sum + float(M - 1 - MaxI).


zero_count({T, M}) ->
    M - gb_trees:size(T).

compact({T, M}) ->
    {T, M}.

encode_registers({T, M}) ->
    iolist_to_binary(
      encode_registers(M-1, T, [])).

encode_registers(I, _T, ByteEncoded) when I < 0 ->
    ByteEncoded;

encode_registers(I, T, ByteEncoded) when I >= 0 ->
    Byte = case gb_trees:lookup(I, T) of
               {value, V} ->
                   <<V:8/integer>>;
               none ->
                   <<0>>
           end,
    encode_registers(I - 1, T, [Byte | ByteEncoded]).


decode_registers(Bytes, P) ->
    L = do_decode_registers(Bytes, 0),
    T = gb_trees:from_orddict(L),
    M = trunc(math:pow(2, P)),
    {T, M}.


do_decode_registers(<<>>, _) ->
    [];
do_decode_registers(<<0:8/integer, Rest/binary>>, I) ->
    do_decode_registers(Rest, I+1);
do_decode_registers(<<Value:8/integer, Rest/binary>>, I) ->
    [{I, Value} | do_decode_registers(Rest, I+1)].

%%
%% TESTS
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

sum_test() ->
    T = set(3, 5, set(1, 1, new(4))),

    ?assertEqual(lists:sum([
                            math:pow(2, -0), % 0
                            math:pow(2, -1), % 1
                            math:pow(2, -0), % 2
                            math:pow(2, -5), % 3
                            math:pow(2, -0), % 4
                            math:pow(2, -0), % 5
                            math:pow(2, -0), % 6
                            math:pow(2, -0), % 7
                            math:pow(2, -0), % 8
                            math:pow(2, -0), % 9
                            math:pow(2, -0), % 10
                            math:pow(2, -0), % 11
                            math:pow(2, -0), % 12
                            math:pow(2, -0), % 13
                            math:pow(2, -0), % 14
                            math:pow(2, -0)  % 15
                           ]),
                 register_sum(T)).

zero_test() ->
    P = 4, M = 16,
    T = set(3, 5, set(1, 1, new(P))),
    ?assertEqual(M - 2, zero_count(T)).

-endif.
