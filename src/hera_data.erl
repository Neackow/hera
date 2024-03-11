-module(hera_data).

-behaviour(gen_server).

-export([start_link/0]).
-export([get/1, get/2]).
-export([store/4]).
-export([init/1, handle_call/3, handle_cast/2]).

-type measure() :: {node(), pos_integer(), hera:timestamp(), [number(), ...]}.

-export_type([measure/0]).

-record(data, {
    seq = 0 :: non_neg_integer(),
    values :: [number(), ...] | undefined,
    timestamp :: hera:timestamp(),
    file :: string() | undefined
}).


% Decide whether or not to print the comments. Remember to change it in your environment.
output_log(Message, Args=[]) ->
    ShowLogs = application:get_env(hera, show_log, false), 
    if 
        ShowLogs -> 
            io:format(Message,Args);
        true -> 
            ok
    end.

% Decide whether or not to print the comments. Remember to change it in your environment.
output_log_spec(Message, Args=[]) ->
    ShowLogs = application:get_env(hera, show_log_spec, false), 
    if 
        ShowLogs -> 
            io:format(Message,Args);
        true -> 
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec get(Name) -> Measures when
    Name :: atom(),
    Measures :: [measure()].

get(Name) ->
    gen_server:call(?MODULE, {get, Name}).


-spec get(Name, Node) -> [Measure] when 
    Name :: atom(),
    Node :: node(),
    Measure :: measure().

get(Name, Node) -> 
    gen_server:call(?MODULE, {get, Name, Node}).


-spec store(Name, Node, Seq, Values) -> ok when
    Name :: atom(),
    Node :: node(),
    Seq :: pos_integer(),
    Values :: [number(), ...].

store(Name, Node, Seq, Values) ->

    % For debugging purposes.
    output_log("hera_data:store has been reached!~n",[]),

    gen_server:cast(?MODULE, {store, Name, Node, Seq, Values}).  % This is a typical cast call, which is handled by handle_cast.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, #{}}.


handle_call({get, Name}, _From, MapData) ->

    % For debugging purposes.
    output_log_spec("hera_data:handle_call (Name alone version) has been reached! Dealing with it.~n",[]),

    MapMeasure = maps:get(Name, MapData, #{}),
    output_log_spec("Is MapMeasure = maps:get(Name, MapData, #{}) taking 5secs?~n",[]),

    L = maps:to_list(MapMeasure),
    output_log_spec("Is L = maps:to_list(MapMeasure) taking 5secs?~n",[]),

    Res = [{Node,S,T,V} || {Node, #data{seq=S,values=V,timestamp=T}} <- L],
    output_log_spec("Is the reply taking 5secs?~n",[]),

    {reply, Res, MapData};


handle_call({get, Name, Node}, _From, MapData) ->

    % For debugging purposes.
    output_log_spec("hera_data:handle_call (Name,Node version) has been reached! Dealing with it.~n",[]),

    MapMeasure = maps:get(Name, MapData, #{}),
    output_log_spec("Is MapMeasure = maps:get(Name, MapData, #{}) taking 5secs?~n",[]),

    Res = if
        is_map_key(Node, MapMeasure) ->
            #data{seq=S,values=V,timestamp=T} = maps:get(Node, MapMeasure),
            output_log_spec("Is #data{seq=S,values=V,timestamp=T} = maps:get(Node, MapMeasure) from is_map_key(Node, MapMeasuer) taking 5secs?~n",[]),

            [{Node,S,T,V}];
        true ->
            []
    end,
    output_log_spec("Is the reply taking 5secs?~n",[]),

    {reply, Res, MapData};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({store, Name, Node, Seq1, L}, MapData) ->

    % For debugging purposes.
    output_log("hera_data:store is being handled by handle_cast!~n",[]),
    
    MapNode0 = maps:get(Name, MapData, #{}),
    IsLogger = application:get_env(hera, log_data, true), 
    % Here, due to the fact that we defined true in the configuration files, IsLogger should be true.
    % io:format("IsLogger is ~p~n~n~n", [IsLogger]),
    MapNode1 = if
        is_map_key(Node, MapNode0) ->
            MapNode0;
        IsLogger ->
            File = file_name(Name, Node),
            MapNode0#{Node => #data{file=File}};
        true ->
            MapNode0#{Node => #data{}}
    end,
    Data = maps:get(Node, MapNode1),
    MapNode2 = case Data of
        #data{seq=Seq0} when Seq0 < Seq1 ->
            T = hera:timestamp(),

            % For debugging purposes.
            output_log("hera_data:handle_cast is calling log_data!~n",[]),

            log_data(Data#data.file, {Seq1, T, L}, IsLogger),       % Eventually, this seems to write to the csv.
            NewData = Data#data{seq=Seq1,values=L,timestamp=T},
            maps:put(Node, NewData, MapNode1);
        _ ->
            MapNode1
    end,
    {noreply, maps:put(Name, MapNode2, MapData)};

handle_cast(_Request, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_name(Name, Node) ->
    lists:append(
        ["measures/", atom_to_list(Name), "_", atom_to_list(Node), ".csv"]).


log_data(_, _, false) ->

    % For debugging purposes.
    output_log("FALSE VERSION of hera_data:log_data has been reached!~n",[]),

    ok;

log_data(File, {Seq, T, Ms}, true) ->

    % For debugging purposes.
    output_log("hera_data:log_data has been reached!~n",[]),

    Vals = lists:map(fun(V) -> lists:flatten(io_lib:format("~p", [V])) end, Ms),
    S = string:join(Vals, ","),
    Bytes = io_lib:format("~p,~p,~s~n", [Seq, T, S]),

    % For debugging purposes.
    output_log("hera_data:log_data should be verifying that measures/ exists or will create it!~n",[]),

    ok = filelib:ensure_dir("measures/"),

    % For debugging purposes.
    output_log("hera_data:log_data should be writting!~n",[]),

    ok = file:write_file(File, Bytes, [append]).
