-module(kcolor).

-include_lib("eunit/include/eunit.hrl").

-export([add_edge/3, add_vertex/2, colors_of/2,
	 empty_graph/0, kcolor/2, kcolor_algorithm/2,
	 not_intersection/2, parse_graph/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% KCOLOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kcolor(L, N) ->
    G = parse_graph(L),
    Colors = ncolors(G),
    {Vertexes, _} = G,
    case lists:max(colors_of(Colors, Vertexes)) =< N of
      true -> number2char(Colors);
      false -> false
    end.

number2char([]) -> [];
number2char([{C, V} | Colors]) ->
    [{V, lists:nth(C, alphabet())}] ++ number2char(Colors).

alphabet() ->
    ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
     "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v",
     "w", "x", "y", "z"].

ncolors({[], _}) -> 0;
ncolors({[V | Vertexes], E}) ->
    Colors = [{V, 1}],
    kcolor_algorithm({Vertexes, E}, Colors).

kcolor_algorithm({[], _}, Colors) -> Colors;
kcolor_algorithm({[V | Vertexes], Edges}, Colors) ->
    Neighbors = neighbors({[V | Vertexes], Edges}, V),
    CurrentColors = colors_of(Colors, Neighbors),
    NewColors = case length(CurrentColors) == 0 of
		  true -> Colors ++ [{1, V}];
		  false ->
		      MissingValues = not_intersection(lists:seq(1,
								 lists:max(CurrentColors)),
						       CurrentColors),
		      case length(MissingValues) == 0 of
			true -> Colors ++ [{lists:max(CurrentColors) + 1, V}];
			false -> Colors ++ [{lists:nth(1, MissingValues), V}]
		      end
		end,
    kcolor_algorithm({Vertexes, Edges}, NewColors).

colors_of(Colors, List) ->
    lists:map(fun ({C, _}) -> C end,
	      lists:filter(fun ({_, V}) -> lists:member(V, List) end,
			   Colors)).

not_intersection([], _) -> [];
not_intersection([V | L1], L2) ->
    case lists:member(V, L2) of
      false -> [V] ++ not_intersection(L1, L2);
      true -> not_intersection(L1, L2)
    end.

parse_graph(G) ->
    Vertexes = lists:map(fun ({V, _}) -> V end, G),
    Edges = lists:flatten(lists:map(fun ({V, E}) ->
					    [{V, E2} || E2 <- E]
				    end,
				    G)),
    add_edges(add_vertexes(empty_graph(), Vertexes), Edges).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END KCOLOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GRAPH IMPLEMENTATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_graph() -> {[], []}.

add_vertex({Vertexes, Edges}, V) ->
    case lists:member(V, Vertexes) of
      false -> {Vertexes ++ [V], Edges};
      true -> {Vertexes, Edges}
    end.

add_edge({Vertexes, Edges}, V1, V2) ->
    case lists:member(V1, Vertexes) and
	   lists:member(V2, Vertexes)
	   and not lists:member({V1, V2}, Edges)
	   and not lists:member({V2, V1}, Edges)
	   % Not reflexive connection allow
	   and
	   (V1 =/= V2)
	of
      true -> {Vertexes, Edges ++ [{V1, V2}]};
      false -> {Vertexes, Edges}
    end.

neighbors({_, []}, _) -> [];
neighbors({Vertexes, [{V1, V2} | Edges]}, V)
    when V1 == V ->
    [V2] ++ neighbors({Vertexes, Edges}, V);
neighbors({Vertexes, [{V1, V2} | Edges]}, V)
    when V2 == V ->
    [V1] ++ neighbors({Vertexes, Edges}, V);
neighbors({Vertexes, [_ | Edges]}, V) ->
    neighbors({Vertexes, Edges}, V).

add_vertexes(G, []) -> G;
add_vertexes(G, [V1 | V]) ->
    add_vertexes(add_vertex(G, V1), V).

add_edges(G, []) -> G;
add_edges(G, [{V1, V2} | V]) ->
    add_edges(add_edge(G, V1, V2), V).

build_graph(V, E) ->
    add_edges(add_vertexes(empty_graph(), V), E).

main_test() ->
    {ok, GraphCases} =
	file:consult("test_cases_graph_impl.txt"),
    {ok, ParseCases} =
	file:consult("test_cases_graph_parse.txt"),
    {ok, NeighborsCases} =
	file:consult("test_cases_graph_neighbors.txt"),
    {ok, KcolorCases} =
	file:consult("test_cases_kcolor.txt"),
    lists:map(fun ({V, E, OK1}) -> OK1 = build_graph(V, E)
	      end,
	      GraphCases),
    lists:map(fun ({Input, OK2}) -> OK2 = parse_graph(Input)
	      end,
	      ParseCases),
    lists:map(fun ({G, V, OK3}) -> OK3 = neighbors(G, V)
	      end,
	      NeighborsCases),
    lists:map(fun ({G, N, OK4}) -> OK4 = kcolor(G, N) end,
	      KcolorCases).
