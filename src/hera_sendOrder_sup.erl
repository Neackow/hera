-module(hera_sendOrder_sup).
-behaviour(supervisor).
 
-export([start_link/0]).
-export([init/1]).


start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => hera_sendOrder,
                    start => {hera_sendOrder, start_link, []},
                    restart => transient,
                    shutdown => 5000,
                    type => worker,
                    modules => [hera_sendOrder]}],  
    {ok, {SupFlags, ChildSpecs}}.

