% Based on: http://learnyousomeerlang.com

-module(sat_server).

-export([allow_more_children/0, create_listeners/0,
	 init/1, start_link/0]).

-define(PORT, 3547).

-define(MAX_CHILDREN, 8).

% Initialize server
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = {ok, ?PORT},
    {ok, ListenSocket} = gen_tcp:listen(Port,
					[{active, once}, {packet, line},
					 {reuseaddr, true}]),
    spawn_link(fun empty_listeners/0),
    % https://erlang.org/doc/design_principles/sup_princ.html#example
    SupFlags = #{strategy => simple_one_for_one,
		 intensity => 60, period => 3600},
    ChildSpecs = [#{id => socket,
		    start => {listener, start_link, [ListenSocket]},
		    shutdown => 100 * 10, type => worker,
		    restart => temporary, modules => [listener]}],
    {ok, {SupFlags, ChildSpecs}}.

% Creates empty listeners, can be used to create multiple listeners
empty_listeners() ->
    supervisor:start_child(?MODULE, []), ok.

% Alias for start_socket().
create_listeners() ->
    supervisor:start_child(?MODULE, []).

allow_more_children() ->
    length(supervisor:which_children(?MODULE)) =<
      (?MAX_CHILDREN).
