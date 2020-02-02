-module(listener).

-record(state, {socket, worker}).

-export([start_link/1, stop/0]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

% Starts the server
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

% Converts a string to an atom
parse(S) ->
    try {ok, Tokens, _} =
	    erl_scan:string(string:concat(re:replace(S, "\r\n$", "",
						     [{return, list}]),
					  ".")),
	{ok, Value} = erl_parse:parse_term(Tokens),
	Value
    catch
      _:_ -> error
    end.

% Initializes the server
init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket = Socket}}.

% Handles all calls
handle_call(_, _, State) -> {noreply, State}.

% Accepts a connection and decides if busy or not

handle_cast(accept,
	    S = #state{socket = Sock_Listener}) ->
    {ok, Socket} = gen_tcp:accept(Sock_Listener),
    {Msg, Response} = case sat_server:allow_more_children()
			  of
			true ->
			    {"ready",
			     {noreply, S#state{socket = Socket}, 60000}};
			false -> {"busy", {stop, normal, S}}
		      end,
    send(Socket, Msg, []),
    sat_server:create_listeners(),
    Response.

% If TCP is closed, it stops
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};
% If timeout is sent, it sends a message of "trying"
handle_info(timeout, S = #state{worker = Worker})
    when Worker =:= undefined ->
    {stop, normal, S};
handle_info(timeout, S = #state{socket = Socket}) ->
    send(Socket, "trying", []), {noreply, S, 7500};
% If worker ignores, then it sends "ignored"
handle_info({worker, ignored},
	    _S = #state{worker = Worker, socket = Socket}) ->
    exit(Worker, worker_ignored),
    send(Socket, "ignored", []),
    {noreply, #state{socket = Socket, worker = undefined},
     60000};
% If worker is trying, then continue and don't do anything else
handle_info({worker, trying}, S) -> {noreply, S, 0};
% If worker returns a result, it sends the result and reset time
handle_info({worker, result, Info},
	    _S = #state{worker = Worker, socket = Socket}) ->
    exit(Worker, worker_down),
    send(Socket, "~p", [Info]),
    {noreply, #state{socket = Socket, worker = undefined},
     60000};
% Decides if spawn a worker, to abort or ignore
handle_info({tcp, _, Raw},
	    State = #state{socket = Sock_Listener, worker = W}) ->
    Data = try parse(Raw) catch _:_ -> error end,
    case W of
      undefined ->
	  Worker = spawn_link(sat, execute_expression,
			      [Data, self()]),
	  {noreply,
	   #state{socket = Sock_Listener, worker = Worker}, 7500};
      _ ->
	  case Data of
	    abort ->
		exit(W, worker_down),
		send(Sock_Listener, "aborted", []),
		{noreply,
		 #state{socket = Sock_Listener, worker = undefined}};
	    _ ->
		send(Sock_Listener, "ignored", []),
		{noreply, State, 7500}
	  end
    end;
% Any other, do nothing
handle_info(_, S) -> {noreply, S}.

code_change(_, S, _) -> {ok, S}.

terminate(normal, _) -> ok;
terminate(_, _) -> error.

stop() -> gen_server:cast(?MODULE, stop).

%  It sends information to sockets
send(Socket, Sol, Options) ->
    gen_tcp:send(Socket,
		 io_lib:format(Sol ++ "~n", Options)),
    inet:setopts(Socket, [{active, true}]),
    ok.
