-module(hera_com).

-export([start_link/0]).
-export([send/3]).

-define(MULTICAST_ADDR, {224,0,2,15}).
-define(MULTICAST_PORT, 62476).



% Decide whether or not to print the comments. Remember to change it in your environment.
output_log(Message, Args) ->
    ShowLogs = application:get_env(hera, show_log, false), 
    if 
        ShowLogs -> 
            io:format(Message,Args);
        true -> 
            ok
    end.

output_log_spec(Message, Args) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_datetime(erlang:timestamp()),
    DisplayedTime = list_to_binary(io_lib:format("~.2.0w:~.2.0w:~.2.0w", [Hour, Min, Sec])),

    ShowLogs = application:get_env(hera, show_log_spec, false), 
    if
        ShowLogs -> 
            if Args == [] ->
                io:format("[~p]: ~p.~n",[DisplayedTime, Message]);
               true -> 
                FullMessage = "[~p]: " ++ Message,
                io:format(FullMessage, [DisplayedTime|Args])
            end;
        true -> 
            ok
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    Pid = spawn_link(fun init/0),
    register(?MODULE, Pid),
    {ok, Pid}.


-spec send(Name, Seq, Values) -> ok when
    Name :: atom(),
    Seq :: pos_integer(),
    Values :: [number(), ...].

send(Name, Seq, Values) ->

    % For debugging purposes.
    output_log("hera_com:send has been reached!~n",[]),

    Message = {hera_data, Name, node(), Seq, Values},
    try ?MODULE ! {send_packet, term_to_binary(Message)} % it will try to send to itself. This goes to the loop(Socket) function.
    catch
        error:_ -> ok
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
    Socket = open_socket(1),
    io:format("Connection established!~n"),
    loop(Socket). 


open_socket(Delay) ->
    try open_socket()
    catch
        error:Reason ->
            io:format("Could not open socket:~p~n", [Reason]),
            io:format("Retrying in ~p [s]~n", [Delay]),
            timer:sleep(Delay*1000),
            open_socket(min(2*Delay, 60))
    end.


open_socket() ->
    {ok, Addrs} = inet:getifaddrs(),
    OwnAddr = hd([
        Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
        size(Addr) == 4, Addr =/= {127,0,0,1}
    ]),
    {ok, Socket} = gen_udp:open(?MULTICAST_PORT, [
        binary,
        inet,
        {active, true},
        {multicast_if, OwnAddr},
        {multicast_loop, true},
        {reuseaddr, true},
        {add_membership, {?MULTICAST_ADDR, OwnAddr}}
    ]),
    Socket.


loop(Socket) ->

    % For debugging purposes.
    output_log("hera_com:loop has been reached!~n",[]),

    receive
        {udp, _Sock, _IP, _InPortNo, Packet} ->     % Eventually, we receive the data. This in term calls the store function from hera_data.
            
            % For debugging purposes.
            output_log("I am hera_com:loop(Socket) and I received an udp message!~n",[]),
            
            Message = binary_to_term(Packet),
            case Message of
                {hera_data, Name, From, Seq, Values} ->

                    % For debugging purposes.
                    output_log("I am hera_com:loop(Socket) and I am trying to store data!~n",[]),

                    case Name of 
                        e11 -> 
                            output_log_spec("Call to hera_data:store for values ~p.~n",[Values]);
                        _ ->
                            ok
                    end,

                    hera_data:store(Name, From, Seq, Values);
                _ ->
                    ok
            end;
        {send_packet, Packet} -> % A priori, from the initial call, we get here. This sends the data to all connected node, apparently. Thus, to self too.
            
            % For debugging purposes.
            output_log("I am hera_com:loop(Socket) and I received a send_packet message!~n",[]),
            
            gen_udp:send(Socket, ?MULTICAST_ADDR, ?MULTICAST_PORT, Packet);
        _ ->
            ok
    end,
    loop(Socket).
