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

start_link(Module, Args) ->

    % For debugging purposes.
    output_log("Hey, I'm hera_measure, this is my start_link function!~n",[]),

    Pid = spawn_link(fun() -> init({Module, Args}) end),
    {ok, Pid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% In hera_measure, this starts the init function and calls e11:init(R0), e.g.

init({Mod, Args}) ->

    % For debugging purposes.
    output_log("A hera_measure process is being init!~n",[]),

    {ok, ModState, Spec} = Mod:init(Args), % Here, e11:init(R0) will be called.
    L0 = ?record_to_tuplelist(state, #state{}),
    L1 = lists:map(fun({Key, Val}) -> maps:get(Key, Spec, Val) end, L0),
    State = list_to_tuple([state|L1]),
    Seq = init_seq(State#state.name),
    case State#state.sync of
        true ->
            
            % For debugging purposes.
            output_log("In hera_measure:init, I will subscribe a process (true condition on State#state.sync)!~n",[]),

            PidRef = subscribe(State#state.name), % Here, we have a subscription. The process is monitored.
            NewState =
                State#state{seq=Seq,mod=Mod,mod_state=ModState,monitor=PidRef},
            loop(NewState, true);
        false ->

            % For debugging purposes.
            output_log("In hera_measure:init, in the false condition on State#state.sync)!~n",[]),

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

            hera_com:send(N, Seq, Vals), % This will call hera_com:send(N, Seq, Vals), from the loop function, when the message is authorized.
            NewIter = case Iter of
                infinity -> Iter;
                _ -> Iter-1
            end,
            State#state{seq=Seq+1, iter=NewIter, mod_state=NewMS}
    end.
