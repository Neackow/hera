-module(hera_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link(?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 6,
        period => 3600,
        shutdown => 2000 % Used to give the time to buffered_logger to log the data.
    },
    HeraData = #{
        id => hera_data,
        start => {hera_data, start_link, []}
    },
    HeraCom = #{
        id => hera_com,
        start => {hera_com, start_link, []}
    },
    HeraMeasureSup = #{
        id => hera_measure_sup,
        start => {hera_measure_sup, start_link, []},
        type => supervisor
    },
    HeraBufferedLogger = #{
        id => buffered_logger,
        start => {buffered_logger, start_link, [{2000}]} % 2000 = number of data to be saved in one go. Each 2000 data, save them.
    },
    HeraI2C = #{
        id => hera_i2c_communication,
        start => {hera_i2c_communication, start_link, []}
    },
    ChildSpecs = [HeraData, HeraCom, HeraMeasureSup, HeraBufferedLogger, HeraI2C],
    {ok, {SupFlags, ChildSpecs}}.
