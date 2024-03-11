-module(hera).

-behaviour(application).

-export([start_measure/2, timestamp/0]).
-export([start/2, stop/1]).

-type timestamp() :: integer() | undefined.

-export_type([timestamp/0]).



% Decide whether or not to print the comments. Remember to change it in your environment.
output_log(Message, Args=[]) ->
    ShowLogs = application:get_env(hera, show_log, false), 
    if 
        ShowLogs -> 
            io:format(Message,Args);
        true -> 
            ok
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% starts and supervise the measure defined in the callback module
%% using Module:init(Args)
-spec start_measure(Module, Args) -> {ok, pid()} | {error, term()} when
    Module :: module(),
    Args :: term().

start_measure(Module, Args) ->

    % For debugging purposes.
    output_log("hera:start_measure has been reached!~n",[]),

    hera_measure_sup:start_child(Module, Args). % In hera_measure_sup, this leads to supervisor:start_child(?MODULE, [nav3, Cn]). 


-spec timestamp() -> timestamp().

timestamp() ->
  erlang:monotonic_time(millisecond).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(_StartType, _StartArgs) ->
    hera_sup:start_link().


stop(_State) ->
    ok.
