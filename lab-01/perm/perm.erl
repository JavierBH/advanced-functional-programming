-module(perm).

-export([perm/2]).

-spec perm([T], [T]) -> boolean().

-include_lib("eunit/include/eunit.hrl").

perm(L1, L2) ->
    io:format("~nInput ~p ~nOutput ~p ~n", [L1, L2]),
    perm_aux(lists:reverse(L1), lists:reverse(L2), []).

-spec perm_aux([T], [T], [T]) -> boolean().

perm_aux([], [], []) -> true;
perm_aux(L1, [Item2 | L2], [Item1 | Stack])
    when Item1 =:= Item2 ->
    %io:format("L16 Stack ~p ~p ~p ~n",
    %      [lists:reverse(L1), Stack, L2]),
    perm_aux(L1, L2, Stack);
perm_aux([Item1 | L1], [Item2 | L2], Stack)
    when Item1 =:= Item2 ->
    %io:format("L19 Stack ~p ~p ~p ~n",
    %      [lists:reverse(L1), Stack, L2]),
    perm_aux(L1, L2, Stack);
perm_aux([Item1 | L1], L2, Stack) ->
    %io:format("L19 Stack ~p ~p ~p ~n",
    %      [lists:reverse(L1), [Item1 | Stack], L2]),
    perm_aux(L1, L2, [Item1 | Stack]);
perm_aux(_, _, _) -> false.

perm_test_() ->
    {ok, Cases} = file:consult("test_cases.txt"),
    [?_assertEqual(O, (perm:perm(L1, L2)))
     || {L1, L2, O} <- Cases].

perm_million_test() ->
    [?_assertEqual(true, (perm:perm(L, L)))
     || L <- lists:seq(1, 1000000)].

perm_reversemillion_test() ->
    [?_assertEqual(true, (perm:perm(L, L)))
     || L <- lists:reverse(lists:seq(1, 1000000))].
