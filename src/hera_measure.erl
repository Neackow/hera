-module(hera_measure).

-export([start_link/2]).

-type measure_spec() :: #{
    name := atom(), % measure id
    iter := pos_integer() | infinity, % number of measures to perform
    sync => boolean(), % must the measure must be synchronized? (default: false)
    timeout => timeout() % min delay between two measures (default: 1)
}.

-export_type([measure_spec/0]).

-callback init(Args :: term()) ->
    {ok, State :: term(), Spec :: measure_spec()}.

-callback measure(State :: term()) ->
    {ok, Values, NewState} | {undefined, NewState} when
    Values :: [number(), ...],  
    NewState :: term().

-record(state, {
    name :: atom(),
    sync = false :: boolean(),
    monitor :: {pid(), reference()} | undefined,
    timeout = 1 :: timeout(),
    seq = 1 :: pos_integer(),
    iter = 1 :: non_neg_integer() | infinity,
    mod :: module(),
    mod_state :: term()
}).

-define(record_to_tuplelist(Name, Rec),
    lists:zip(record_info(fields, Name), tl(tuple_to_list(Rec)))).



% Decide whether or not to print the comments. Remember to change it in your environment.
output_log(Message, Args) ->
    ShowLogs = application:get_env(hera, show_log, false), 
    if 
        ShowLogs -> 
            io:format(Message,Args);
        true -> 
            ok
    end.

% Decide whether or not to print the comments. Remember to change it in your environment.
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

start_link(Module, Args) ->

    % For debugging purposes.
    output_log_spec("Hey, I'm hera_measure, this is my start_link function!~n",[]),

    Pid = spawn_link(fun() -> init({Module, Args}) end),
    {ok, Pid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% In hera_measure, this starts the init function and calls e11:init(R0), e.g.

init({Mod, Args}) ->

    % For debugging purposes.
    output_log_spec("A hera_measure process is being init!~n",[]),

    {ok, ModState, Spec} = Mod:init(Args), % Here, e11:init(R0) will be called.
    L0 = ?record_to_tuplelist(state, #state{}),

    output_log_spec("I am L0 : ~p!~n",[L0]),

    L1 = lists:map(fun({Key, Val}) -> maps:get(Key, Spec, Val) end, L0),

    output_log_spec("I am L1 : ~p!~n",[L1]),

    State = list_to_tuple([state|L1]),
    Seq = init_seq(State#state.name),

    output_log_spec("The inital state will be {seq= ~p, mod= ~p, mod_state= ~p, iter= ~p, timeout= ~p}!~n",[Seq, Mod, ModState, State#state.iter, State#state.timeout]),

    case State#state.sync of
        true ->
            
            % For debugging purposes.
            output_log_spec("In hera_measure:init, I will subscribe a process (true condition on State#state.sync)!~n",[]),

            PidRef = subscribe(State#state.name), % Here, we have a subscription. The process is monitored.
            
            output_log_spec("My name is ~p and my Pid is ~p.~n",[State#state.name, PidRef]),

            NewState =
                State#state{seq=Seq,mod=Mod,mod_state=ModState,monitor=PidRef},
            loop(NewState, true);
        false ->

            % For debugging purposes.
            output_log_spec("In hera_measure:init, in the false condition on State#state.sync)!~n",[]),

            NewState = State#state{seq=Seq,mod=Mod,mod_state=ModState},
            loop(NewState, false)
    end.


loop(State, false) ->

    % For debugging purposes.
    output_log("hera_measure:loop with false condition has been reached!~n",[]),

    continue(measure(State));

loop(State=#state{monitor={From,Ref}}, true) ->

    % For debugging purposes.
    output_log("hera_measure:loop with true condition has been reached!~n",[]),

    receive
        {authorized, From} ->
            NewState = measure(State),
            From ! {ok, self()},
            continue(NewState);
        {'DOWN', Ref, _, _, _} ->
            PidRef = subscribe(State#state.name),
            continue(State#state{monitor=PidRef})
    end.


continue(#state{iter=0}) ->
    {stop, normal};

continue(State) ->
    timer:sleep(State#state.timeout),
    loop(State, State#state.sync).


subscribe(Name) ->
    {ok, Pid} = hera_sub:subscribe(Name),
    Ref = monitor(process, Pid),

    % For debugging purposes.
    output_log("Process subscribed (hera_measure)!~n",[]),

    {Pid, Ref}.


%% return 1 or 1 + the last seq number known among all nodes
init_seq(Name) ->
    {ResL, _} = rpc:multicall(hera_data, get, [Name, node()]),
    L = lists:filtermap(fun(Res) ->
        case Res of
            [{_,Seq,_,_}] -> {true, Seq};
            _ -> false
        end
    end, ResL),
    lists:max([0|L]) + 1.


measure(State=#state{name=N, mod=M, mod_state=MS, seq=Seq, iter=Iter}) ->

    % For debugging purposes.
    output_log("hera_measure:measure has been reached!~n",[]),

    case M:measure(MS) of

        {undefined, NewMS} ->

            % For debugging purposes.
            output_log("hera_measure:measure received an undefined response from M:measure(MS)!~n",[]),

            State#state{mod_state=NewMS};
        {ok, Vals=[_|_], NewMS} ->

            % For debugging purposes.
            output_log("Sending to hera_com from hera_measure!~n",[]),

            case M of
                nav3 ->
                    hera_com:send(N, Seq, Vals);
                e11 ->
                    output_log_spec("Hera_measure:measure (e11). Iter is = ~p, calling hera_com:send for values ~p.~n",[Iter, Vals]),
                    hera_com:send(N, Seq, Vals), % This will call hera_com:send(N, Seq, Vals), from the loop function, when the message is authorized.
                    output_log_spec("Hera_measure:measure (e11) after hera_com:send !~n",[])
            end,

            NewIter = case Iter of
                infinity -> Iter;
                _ -> Iter-1
            end,
            State#state{seq=Seq+1, iter=NewIter, mod_state=NewMS}
    end.
