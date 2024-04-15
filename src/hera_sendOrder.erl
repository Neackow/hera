% Nicolas Isenguerre, 13/04/2024. 

% hera module to convert movements to orders on an object. 
% This V1.0 offers the ability to control a wine crate on wheels, with movements associated to defined orders.
% This is called by the set_state_crate(MovementDetected), which calls the 'ctrlCrate' version of the handle_call function.
% Extensions of this code are straightforward: either do your own functions and redirect the orders to it, or extend the handle_call procedures
% for more functionalities. E.g.: if the PMOD Ultrawideband comes out, a follower function can be added. % This could be called via 
% gen_server:call(?MODULE, {follower, WhateverArgument}) and redirect to an appropriate handle_call procedure.

-module(hera_sendOrder).

-behaviour(gen_server).


-export([start_link/0, set_state_crate/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

% At initialisation (see init/1 function): currentSpeed = 100 RPM (in forward direction). 
% movName: store the movement name as detected by the GRiSP board. 
% movMode: this is to deal with submodes. E.g.: if I want to be in 'changeSpeed mode', then it stores this mode. 
% By default: noMove and normal, normal being the 'no movement mode currently'.
-record(movState, {currentSpeed, movName, movMode}).


% =======================================================================
% ========================= <public functions> ==========================
% =======================================================================

start_link() ->
    % Start controlling the object crate_on_wheel with the state 'no_move'. Calls init/1.
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Synchronous call: sets the new state according to the detected movement. This function is called by rpc:call on the other GRiSP. 
set_state_crate(MovementDetected) -> 
    gen_server:call(?MODULE, {ctrlCrate, MovementDetected}).

% =======================================================================
% ========================= </public functions> =========================
% =======================================================================


% =======================================================================
% ========================= <private functions> =========================
% =======================================================================


% Structure of Order: [V1, DIR1, V2, DIR2, command_indicator].
% The command indicator allows the controller to know if it's turning, simply moving forward, etc.
% This could have been simply implemented within the controller by comparing speeds, etc. but since it was needed in changeSpeed mode to try it out,
% it may aswell be reused for simplicity.
% command_indicator = 0 -> nothing special, execute order ; 1 -> test speed in changeSpeed mode ; 2 -> crate-on-wheels turning ; 3 -> turning around.
order_crate(State) ->
    Order = case State#movState.movMode of
        changeSpeed ->
            case State#movState.movName of
                noMove ->   % Stop the crate from whatever it was doing. Reset values to baseline.
                    NewState = State#movState{currentSpeed = 100},
                    [0,1,0,0,0];
                accelerate ->   % When we are changing the speed, do not move the crate, by default. Just change the state.
                    if State#movState.currentSpeed == 100 ->
                        NewState = State#movState{currentSpeed = 120};
                    State#movState.currentSpeed == 120 ->
                        NewState = State#movState{currentSpeed = 140};
                    State#movState.currentSpeed >= 140 ->
                        io:format("Cannot accelerate further!~n"),
                        NewState = State#movState{currentSpeed = 140};
                    true ->
                        NewState = State#movState{currentSpeed = State#movState.currentSpeed}
                    end,
                    [0,1,0,0,0];
                decelerate ->
                    if State#movState.currentSpeed == 140 ->
                        NewState = State#movState{currentSpeed = 120};
                    State#movState.currentSpeed == 120 ->
                        NewState = State#movState{currentSpeed = 100};
                    State#movState.currentSpeed =< 100 -> % Just in case the sun wants to play with me.
                        io:format("Cannot decelerate further!~n"),
                        NewState = State#movState{currentSpeed = 100};
                    true ->
                        NewState = State#movState{currentSpeed = State#movState.currentSpeed}
                    end,
                    [0,1,0,0,0];
                testingSpeed ->
                    NewState = State,
                    [State#movState.currentSpeed,1,State#movState.currentSpeed,0,1];
                _ ->
                    NewState = State,
                    io:format("Invalid command when in changeSpeed mode.~n"),
                    [0,1,0,0,0] % When order is invalid, automatically set to 0.
            end;
        normal ->
            NewState = State,
            case State#movState.movName of
                noMove ->   % Default command, crate does not move.
                    [0,1,0,0,0];
                forward ->
                    [State#movState.currentSpeed,1,State#movState.currentSpeed,0,0];
                backward ->
                    [State#movState.currentSpeed,0,State#movState.currentSpeed,1,0];
                turnLeftForward ->
                    [0,1,State#movState.currentSpeed,0,2];
                turnRightForward ->
                    [State#movState.currentSpeed,1,0,0,2];
                turnLeftBackward ->
                    [0,0,State#movState.currentSpeed,1,2];
                turnRightBackward ->
                    [State#movState.currentSpeed,0,0,1,2];
                turnAround ->
                    io:format("*briiight eyes* EVERY NOW AND THEN I FALL APART!~n"),
                    [100,1,100,1,3]; % Turn on itself, towards the right. Fixed at 100 RPM, could be less, could be fixed to currentSpeed.
                _ -> 
                    io:format("Unknown movement name while in movMode normal.~n"),
                    [0,1,0,0,0]
            end;
        _ ->
            NewState = State,
            io:format("Bad movement mode, crate will not move as a security measure."),
            [0,1,0,0,0]
    end,

    % Send command to the micro-controller.
    %send_i2c(Order),
    io:format("Order is ~p~n", [Order]),
    io:format("Current state is ~p~n", [NewState]),
    NewState.

% Send the command using the I2C ports of the GRiSP. Thank you Cédric Ponsard for this piece of code! 
% For more details on this port: https://digilent.com/blog/new-i2c-standard-for-pmods/
send_i2c(Command) ->
    List_data = lists:flatten(lists:map(fun(X) -> X end, lists:map(fun(X) -> lists:sublist(binary_to_list(term_to_binary(X)),3,8) end, Command))),
    I2C0 = grisp_i2c:open(i2c1),
    grisp_i2c:transfer(I2C0, [{write, 16#40, 1, List_data}]), % 16#40 is the fixed address of the Raspberry Pi Pico W, in hexadecimal format.
    io:format("Command sent to the micro-controller!~n").

read_i2c() ->
    I2C1 = grisp_i2c:open(i2c1),
    Message = grisp_i2c:transfer(I2C1, [{read, 16#40, 1, 1}]), % Reads 1 byte from slave at address 0x40, in register 1.
    io:format("Message received is ~p~n", [Message]),
    Available = lists:nth(1, binary_to_list(lists:nth(1, Message))). % Convert the binary coded on 8 bits and received as [<<val>>] to an integer.
    

% =======================================================================
% ======================== </private functions> =========================
% =======================================================================


% =======================================================================
% ======================= <gen_server functions> ========================
% =======================================================================

init([]) ->
    process_flag(trap_exit, true), % Not really useful if I don't need to deal with something when the gen_server goes down.
    % Display message to the console: allows to see if the function is correctly being setup from the shell.
    io:format("Object controller is being setup!~n"),
    % Change LED colors: allow to visually tell if the process has been launched, or not.
    %grisp_led:color(1,aqua),
    %grisp_led:color(2,yellow),
    % Set default state and return {ok, state}.
    {ok, #movState{currentSpeed = 100, movName = noMove, movMode = normal}}.

handle_call({ctrlCrate, MovementDetected}, From, State = #movState{currentSpeed = CurrentSpeed, movName = MovName, movMode = MovMode}) ->
    %Available = read_i2c(),
    Available = 1,
    if Available == 1 ->
        if MovementDetected == changeSpeed -> 
            NewState = State#movState{movName = noMove, movMode = changeSpeed}; 
            % When entering changeSpeed mode, stop the crate. It allows easy reset of the changeSpeed mode, in case the user is lost.
        MovementDetected == exitChangeSpeed ->
            NewState = State#movState{movName = noMove, movMode = normal};
            % When exiting changeSpeed mode, stop the crate, once again for security measures. 
        true ->
            NewState = State#movState{movName = MovementDetected}
        end,
        FinalNewState = order_crate(NewState);
    true -> 
        io:format("The controller is currently unavailable. Please wait.~n"),
        FinalNewState = State
    end,
    {reply, ok, FinalNewState};

handle_call(stop, From, State = #movState{}) ->
    io:format("Handling stop.~n", []),
    {stop, normal, ok, State}.


handle_cast(Request, State) ->
    {reply, ok, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.


terminate(normal, State) ->
    io:format("hera_sendOrder: normal termination~n",[]);

terminate(shutdown, State = #movState{currentSpeed=CurrentSpeed,movName=MovName,movMode=MovMode}) ->
    % terminating
    io:format("hera_sendOrder: managing shutdown~n",[]);
    
terminate(Reason, State) ->
    io:format("hera_sendOrder: other termination with reason: ~p~n", [Reason]).

% =======================================================================
% ====================== </gen_server functions> ========================
% =======================================================================