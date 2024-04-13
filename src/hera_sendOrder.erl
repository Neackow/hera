% hera module to convert movements to orders on an object. 
% This V1.0 offers the ability to control a wine crate on wheels, with movements associated to defined orders.
% Extensions of this code are straightforward: either add your own function and redirect the orders to it, or extend the order_crate function
% for more functionalities. E.g.: if the PMOD Ultrawideband comes out, a follower function can be added. 

-module(hera_sendOrder).
-behaviour(gen_server).


-export([start_link/0, set_state/2]).   % SET INPUT NUMBERS. send_i2c/2, order_crate/0 I believe I don't need to export them. 
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

% At initialisation (set init/1 function): previousSpeed = currentSpeed = 100 RPM (in forward direction). 
% movName : store the movement name as detected by the GRiSP board. 
% movMode : this is to deal with submodes. E.g.: if I want to be in 'changeSpeed mode', then this stores this will. 
% By default: noMove and normal, normal being the 'no movement mode currently'.
-record(movState, {previousSpeed, currentSpeed, movName, movMode}).


% ========================= <public functions> =========================

start_link() ->
    % Start controlling the object crate_on_wheel with the state 'no_move'. Calls init/1.
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Synchronous call: sets the new state according to the detected movement. This function is called by rpc:call. 
set_state(MovementDetected) -> 
    gen_server:call(?MODULE, {control, MovementDetected}).


% ========================= </public functions> =========================

% ========================= <private functions> =========================

order_crate(State) ->
    Order = case State#movState.movMode of
        changeSpeed ->
            case State#movState.movName of
                noMove ->   % Stop the crate from whatever it was doing.
                    [0,1,0,0];
                accelerate ->   % When we are changing the speed, do not move the crate, by default.
                    if State#movState.currentSpeed == 100 ->
                        NewState = State#movState{previousSpeed = 100, currentSpeed = 120},
                        [0,1,0,0];
                    State#movState.currentSpeed == 120 ->
                        NewState = State#movState{previousSpeed = 120, currentSpeed = 140},
                        [0,1,0,0];
                    State#movState.currentSpeed == 140 ->
                        io:format("Cannot accelerate further!~n"),
                        NewState = State#movState{previousSpeed = 140, currentSpeed = 140},
                        [0,1,0,0];
                    true ->
                        NewState = State#movState{previousSpeed = State#movState.previousSpeed, currentSpeed = State#movState.currentSpeed},
                        [0,1,0,0]
                    end;
                decelerate ->
                    if State#movState.currentSpeed == 140 ->
                        NewState = State#movState{previousSpeed = 140, currentSpeed = 120},
                        [0,1,0,0];
                    State#movState.currentSpeed == 120 ->
                        NewState = State#movState{previousSpeed = 120, currentSpeed = 100},
                        [0,1,0,0];
                    State#movState.currentSpeed == 100 ->
                        io:format("Cannot decelerate further!~n"),
                        NewState = State#movState{previousSpeed = 100, currentSpeed = 100},
                        [0,1,0,0];
                    true ->
                        NewState = State#movState{previousSpeed = State#movState.previousSpeed, currentSpeed = State#movState.currentSpeed},
                        [0,1,0,0]
                    end;
                testingSpeed ->
                    NewState = State,
                    [State#movState.currentSpeed,1,State#movState.currentSpeed,0]
            end;
        exitChangeSpeed -> % When we leave the changeSpeed mode, ensure that the crate does not move.
            [0,1,0,0];
        normal ->
            % TO DO.
        _ ->
            io:format("Bad movement mode, crate will not move as a security measure"),
            [0,1,0,0]
    end,

    send_i2c(Order),
    NewState.

send_i2c(Command) ->
    ok.


% ========================= </private functions> =========================

% ======================== <gen_server functions> ========================

init([]) ->
    process_flag(trap_exit, true),
    % Display message to the console: allows to see if the function is correctly being setup from the shell.
    io:format("Object controller is being setup!~n"),
    % Change LED colors: allow to visually tell if the process has been launched, or not.
    grisp_led:color(1,aqua),
    grisp_led:color(2,yellow),
    % Set default state and return {ok, state}.
    {ok, #movState{previousSpeed = 100, currentSpeed = 100, movName = noMove, movMode = normal}}.

handle_call({control, MovementDetected}, From, State = #movState{previousSpeed=PreviousSpeed,currentSpeed=CurrentSpeed,movName=MovName,movMode=MovMode}) ->
    if MovementDetected == changeSpeed -> 
        NewState = State#movState{movName = noMove, movMode = changeSpeed}, 
        % When entering changeSpeed mode, stop the crate. It allows easy reset of the changeSpeed mode, in case the user is lost.
        FinalNewState = order_crate(NewState);
    MovementDetected == exitChangeSpeed ->
        NewState = State#movState{movName = noMove, movMode = exitChangeSpeed}, 
        % When exiting changeSpeed mode, stop the crate. 
        FinalNewState = order_crate(NewState);
    true ->
        NewState = State#movState{movName = MovementDetected},
        FinalNewState = order_crate(NewState)
    end,
    {reply, ok, FinalNewState};

handle_call(stop, From, State = #movState{}) ->
    io:format("Handling stop ~n", []),
    {stop, normal, ok, State}.


handle_cast(Request, State) ->
    {reply, ok, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.


terminate(normal, State) ->
    io:format("hera_sendOrder: normal termination~n",[]);

terminate(shutdown, State = #movState{previousSpeed=PreviousSpeed,currentSpeed=CurrentSpeed,movName=MovName,movMode=MovMode}) ->
    % terminating
    io:format("hera_sendOrder: managing shutdown~n",[]);
    
terminate(Reason, S) ->
    io:format("hera_sendOrder: other termination", [Reason]).

% ======================= </gen_server functions> ========================