-module(sat).

-export([execute_expression/2, get_result/1,
	 instances_test/0]).

validate(Expressions) ->
    try case Expressions of
	  [] -> false;
	  _ -> validate_expression(Expressions)
	end
    catch
      _:_ -> false
    end.

validate_expression([]) -> true;
validate_expression([{X, Y, Z} | E])
    when is_integer(X), is_integer(Y), is_integer(Z),
	 X =/= 0, Y =/= 0, Z =/= 0 ->
    validate_expression(E);
validate_expression(_) -> false.

max_number([], V) -> V;
max_number([{Xr, Yr, Zr} | E], V) ->
    {X, Y, Z} = {abs(Xr), abs(Yr), abs(Zr)},
    if X > V, X > Y, X > Z -> max_number(E, X);
       Y > V, Y > Z -> max_number(E, Y);
       Z > V -> max_number(E, Z);
       true -> max_number(E, V)
    end.

truth_table(0) -> [[]];
truth_table(N) ->
    [[X | Y]
     || X <- [true, false], Y <- truth_table(N - 1)].

execute_expression(Expressions, Parent) ->
    case validate(Expressions) of
      false -> Parent ! {worker, ignored};
      true ->
	  Parent ! {worker, trying},
	  Parent ! {worker, result, get_result(Expressions)}
    end.

get_result(Expressions) ->
    Max = max_number(Expressions, 0),
    check_options(Expressions, truth_table(Max)).

% Checks all permutations
check_options(Expressions, Options) ->
    AllValid = lists:filtermap(fun (Option) ->
				       case is_valid(Expressions, Option) of
					 true -> {true, Option};
					 _ -> false
				       end
			       end,
			       Options),
    case length(AllValid) of
      0 -> unsat;
      _ -> {sat, lists:nth(1, AllValid)}
    end.

% Tests if the Expression is true
is_valid([], _) -> true;
is_valid([{X, Y, Z} | T], Option) ->
    (get_bool(X, Option) or get_bool(Y, Option) or
       get_bool(Z, Option))
      and is_valid(T, Option).

get_bool(N, Option) ->
    V = lists:nth(abs(N), Option),
    case N < 0 of
      true -> not V;
      _ -> V
    end.

testu(Expressions) ->
    Result = get_result(Expressions),
    case Result of
      unsat -> unsat;
      {sat, Assignment} -> length(Assignment)
    end.

get_length(Output, _) when Output =:= unsat -> unsat;
get_length(_, E) -> max_number(E, 0).

assertEqual(A, B) when A =/= B -> io:format("error~n");
assertEqual(_, _) -> io:format("ok~n").

instances_test() ->
    {ok, Cases} = file:consult("instances.txt"),
    [assertEqual(get_length(Output, Expressions),
		 testu(Expressions))
     || {Output, Expressions} <- Cases].
