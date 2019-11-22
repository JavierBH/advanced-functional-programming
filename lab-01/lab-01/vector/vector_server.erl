-module(vector_server).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-export([evaluate/1, start_link/0]).

-define(SERVER, ?MODULE).

-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).

evaluate(S) ->
    % http://erlang.org/pipermail/erlang-questions/2005-April/015276.html
    {ok, Tokens, _EndLine} =
	erl_scan:string(string:concat(S, ".")),
    {ok, Value} = erl_parse:parse_term(Tokens),
    evaluate_expression(Value, -1).

evaluate_expression(_, N) when N >= 100 -> "error";
evaluate_expression({VectorOP, E1, E2}, N)
    when VectorOP =:= add;
	 VectorOP =:= sub;
	 VectorOP =:= dot ->
    L1 = evaluate_expression(E1, N + 1),
    L2 = evaluate_expression(E2, N + 1),
    case (L1 =/= "error") and (L2 =/= "error") and
	   (length(L1) =:= length(L2))
	of
      true -> make_operation(atom_to_list(VectorOP), L1, L2);
      false -> "error"
    end;
evaluate_expression({ScalarOP, IntExpr, E}, N) ->
    L = evaluate_expression(E, N + 1),
    case L =/= "error" of
      true ->
	  make_operation(atom_to_list(ScalarOP),
			 get_int_expr(IntExpr, L), L);
      false -> "error"
    end;
evaluate_expression(L, _) ->
    case L of
      [] -> [];
      L when is_list(L) ->
	  case (lists:min(L) >= 1) and (lists:max(L) =< 100) of
	    true -> L;
	    false -> "error"
	  end;
      _ -> "error"
    end.

get_int_expr(norm_one, L) ->
    lists:sum(lists:map(fun (A) -> abs(A) end, L));
get_int_expr(norm_inf, L) ->
    lists:max(lists:map(fun (A) -> abs(A) end, L));
get_int_expr(IntExpr, _) -> IntExpr.

make_operation(OP, L1, L2) when OP =:= "add" ->
    [X + Y || {X, Y} <- lists:zip(L1, L2)];
make_operation(OP, L1, L2) when OP =:= "sub" ->
    [X - Y || {X, Y} <- lists:zip(L1, L2)];
make_operation(OP, L1, L2) when OP =:= "dot" ->
    [X * Y || {X, Y} <- lists:zip(L1, L2)];
make_operation(OP, IntExpr, L) when OP =:= "mul" ->
    [X * IntExpr || X <- L];
make_operation(OP, IntExpr, L)
    when (OP =:= "div") and (IntExpr =:= 0) and
	   (length(L) =/= 0) ->
    "error";
make_operation(OP, IntExpr, L) when OP =:= "div" ->
    [X / IntExpr || X <- L];
make_operation(_, _, _) -> "error".

% SERVER
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port],
			  []).

start_link() -> start_link(?DEFAULT_PORT).

% https://github.com/erlware/Erlang-and-OTP-in-Action-Source
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) -> {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply,
     State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
    io:format('~p~n', [LSock]),
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_rpc(Socket, RawData) ->
    try evaluate(RawData) of
      R ->
	  gen_tcp:send(Socket, io_lib:fwrite("Res: ~w~n", [R]))
    catch
      _ -> ok
    end.

main_test_() ->
    {ok, Cases} = file:consult("test_cases_lang.txt"),
    [?_assertEqual(O, (evaluate(E))) || {E, O} <- Cases].
