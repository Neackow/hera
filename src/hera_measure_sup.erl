-module(hera_measure_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/2]).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_child(Module, Args) ->

    io:format("hera_measure_sup:start_child has been reached!~n"),

    supervisor:start_child(?MODULE, [Module, Args]),

    io:format(supervisor:start_child(?MODULE, [Module, Args])).

% The child process is started by using the start function as defined in the child specification (if not simple_one_for_one).
% Here, it is a simple_one_for_one. The child specification defined in Module:init/1 is used (which is "hera_measure"), and ChildSpec must instead be an arbitrary list of terms List. 
% The child process is then started by appending List to the existing start function arguments, that is, by calling apply(M, F, A++List), where {M,F,A} is the start function defined 
% in the child specification.
% So, a hera_measure is launched.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    HeraMeasure = #{
        id => hera_measure,
        start => {hera_measure, start_link, []},
        restart => transient
    },
    ChildSpecs = [HeraMeasure],
    {ok, {SupFlags, ChildSpecs}}.
