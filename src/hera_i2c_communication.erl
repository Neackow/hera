% Nicolas Isenguerre, 04/05/2024. 

% hera module to safely send messages to any slave from a GRiSP.
% This piece of code has been implemented in order to avoid any bus conflict, since multiple functions could be wanting to use the bus at the same
% time to do different tasks.

-module(hera_i2c_communication).

-behaviour(gen_server).


-export([start_link/0, send_data/1, read_data/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

% Store the bus name in the state, to open it once at initialisation and continuously be able to call it.
-record(busI2C, {busName}).

% =======================================================================
% ========================= <public functions> ==========================
% =======================================================================

start_link() ->
    % Initiate an I2C communication.
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_data(Command) ->
    gen_server:cast(?MODULE, {write, Command}).

read_data() ->
    gen_server:call(?MODULE, {read}).

% =======================================================================
% ========================= </public functions> =========================
% =======================================================================


% =======================================================================
% ========================= <private functions> =========================
% =======================================================================

% Send the command using the I2C ports of the GRiSP. Thank you Cédric Ponsard for this piece of code! 
% For more details on this port: https://digilent.com/blog/new-i2c-standard-for-pmods/

send_i2c(Command, Bus) ->
    List_data = lists:flatten(lists:map(fun(X) -> X end, lists:map(fun(X) -> lists:sublist(binary_to_list(term_to_binary(X)),3,8) end, Command))),
    grisp_i2c:transfer(Bus, [{write, 16#40, 1, List_data}]), % 16#40 is the fixed address of the Raspberry Pi Pico W, in hexadecimal format.
    io:format("Command sent to the micro-controller!~n").

% Read from the controller if we are avaiable or not.
read_i2c(Bus) ->
    Message = grisp_i2c:transfer(Bus, [{read, 16#40, 1, 1}]), % Reads 1 byte from slave at address 0x40, in register 1.
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
    io:format("An I2C communication is being setup!~n"),
    I2CBus = grisp_i2c:open(i2c1), % Open the bus.
    % Set default state and return {ok, state}. State is the internal state of the gen_server.
    {ok, #busI2C{busName = I2CBus}}.


% Deal with reading from Raspberry.
handle_call({read}, From, State = #busI2C{busName = BusName}) ->
    FinalState = State,
    Available = read_i2c(BusName),
    {reply, Available, FinalState};

handle_call(stop, From, State = #busI2C{}) ->
    io:format("Stopping I2C communication.~n", []),
    {stop, normal, ok, State}.


% Deal with writing a command to the Raspberry.
handle_cast({write, Command}, State = #busI2C{busName = BusName}) ->
    FinalState = State,
    send_i2c(Command, BusName),
    {noreply, ok, FinalState};

% If anything else.
handle_cast(_Request, State = #busI2C{}) ->
    {noreply, State}.


handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.


terminate(normal, State) ->
    io:format("hera_sendOrder: normal termination~n",[]);

terminate(shutdown, State = #busI2C{busName=BusName}) ->
    % terminating
    io:format("hera_sendOrder: managing shutdown~n",[]);
    
terminate(Reason, State) ->
    io:format("hera_sendOrder: other termination with reason: ~p~n", [Reason]).

% =======================================================================
% ====================== </gen_server functions> ========================
% =======================================================================