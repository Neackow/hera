-module(buffered_logger).

-behaviour(gen_server).


-export([start_link/1, store_record/2, print_all_contents/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

% Values by default for the state of the buffered_logger.
-record(stateBL, {internal_counter=0, max_buffer_size = 200, root_table}).


start_link(Arguments) ->
    % Arguments must have the form {BufferSize = 10}
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arguments, []).

% =======================================================================
% ========================= <public functions> ==========================
% =======================================================================

print_all_contents() ->
    gen_server:call(?MODULE, {print_all_content}).

store_record(TableName, Record) ->
    gen_server:call(?MODULE, {store, TableName, Record}).

% =======================================================================
% ========================= </public functions> =========================
% =======================================================================


% =======================================================================
% ========================= <private functions> =========================
% =======================================================================

create_subtable(SubTableName) ->
    ets:new(SubTableName, [ordered_set]).

insert_data(Table, Key, Record) ->      % It is always {Key, Record}: Key is incremental, Record is the data to be stored. For nav3, Record={Seq, T, Ms}.
    ets:insert(Table, {Key, Record}).


% Print the content of one table. Debugging function.
print_table_records([]) ->
    ok;
print_table_records([H|T]) ->
    {Key, Record} = H,
    io:format("    [~p]: ~p ~n",[Key, Record]),
    print_table_records(T).

% Print table list. Debugging function.
print_table_list([]) ->
    ok;
print_table_list([H|T]) ->
    {Key, Table} = H,
    io:format("Table [~p]: ~n",[Key]),
    TableRecords = ets:tab2list(Table),
    print_table_records(TableRecords),
    print_table_list(T).

% Store table to file. Used when the buffer overflows, or when at the end we flush the rest of the data.
file_name(Name) ->
    lists:append(
        ["measures/", atom_to_list(Name), ".csv"]).

% Adapt this function to be compatible with the measure you want to store.
% Acc = accumulator. SubTable is the sub-directory containing the log, to be added to the file. TableName = name given to SubTable, used for filename.
save_data_to_file(TableName, SubTable) ->

    Content = ets:foldl(fun({Key, Values}, Acc) -> % Here: format the data as before. Values is of the format {Seq, T, Ms} -> use pattern matching.
        {Seq, T, Ms} = Values,
        Vals = lists:map(fun(V) -> lists:flatten(io_lib:format("~p", [V])) end, Ms),
        S = string:join(Vals, ","),
        Bytes = io_lib:format("~p,~p,~s~n", [Seq, T, S]),
        Acc ++ Bytes end, [], SubTable),
    ok = filelib:ensure_dir("measures/"),
    FileName = file_name(TableName),
    ok = file:write_file(FileName, Content, [append]),
    ok.

% =======================================================================
% ========================= </private functions> ========================
% =======================================================================


% =======================================================================
% ======================== <gen_server functions> =======================
% =======================================================================

init(Arguments) ->
    % Create a 'root' table that will store sub-tables
    % Initialize the State with default values

    {MaxBufferSize} = Arguments,
    process_flag(trap_exit, true), % Needed for the flushing when crash down of the app.
    NewRootTable = ets:new(buffered_logger_root,[set, private]),
    % Display success message
    io:format("Root table: ~p; max buffer size: ~p.~n",[NewRootTable, MaxBufferSize]),
    % Return initial state
    {ok, #stateBL{internal_counter = 0, max_buffer_size = MaxBufferSize, root_table = NewRootTable}}.

% When storing the current content in a file.
storeAndEmptyDictionary(RootTable, SubTable, TableName) ->
    %io:format("Storing dictionary on disk: ~p~n",[TableName]),
    save_data_to_file(TableName, SubTable),
    ets:delete(RootTable, TableName).

handle_call({store, TableName, Record}, From, State = #stateBL{internal_counter=InternalCounter, max_buffer_size=MaxBufferSize, root_table=RootTable}) ->
    % Search for TableName in stored tables
    Match = ets:lookup(RootTable, TableName),
    SubTable = 
        if Match =:= [] ->
            % The TableName was not created
            LocalSubTable = create_subtable(TableName),
            insert_data(RootTable, TableName, LocalSubTable),
            LocalSubTable;
        true ->
            % The TableName already exists
            {_, ExistingSubTable} = lists:nth(1, Match),
            ExistingSubTable
    end,

    % The counter is shared between subtables to keep evolution
    NewCounter = InternalCounter + 1,
    insert_data(SubTable, NewCounter, Record),

    % If the sub-table size is > max_buffer_size, store the content on disk and delete from memory

    SubTableSize = ets:info(SubTable, size),
    %io:format("~p size: ~p.~n",[TableName, SubTableSize]),

    StoreResult = 
        if SubTableSize >= MaxBufferSize ->
            storeAndEmptyDictionary(RootTable, SubTable, TableName);
        true ->
            ok
    end,

    % Return the updated state

    {reply, ok, #stateBL{internal_counter = NewCounter, max_buffer_size = MaxBufferSize, root_table = RootTable}};

handle_call({print_all_content}, From, State = #stateBL{internal_counter=InternalCounter, max_buffer_size = MaxBufferSize, root_table=RootTable}) ->
    AllTableList = ets:tab2list(RootTable),
    print_table_list(AllTableList),
    {reply, ok, State};

handle_call(stop, From, State=#stateBL{}) ->
    io:format("Handling stop ~n", []),
    {stop, normal, ok, State}.

handle_cast(Request, State) ->
    {reply, ok, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, S) ->
    io:format("Buffered_logger: normal termination~n",[]);

terminate(shutdown, State = #stateBL{internal_counter=InternalCounter, max_buffer_size = MaxBufferSize, root_table=RootTable}) ->
    % Terminating
    % Store each remaining dictionary on disk
    io:format("Buffered_logger: managing shutdown~n",[]),
    TableList = ets:tab2list(RootTable),
    lists:foreach(fun(LocDictionaryEntry) ->
                {TableName, SubDictionary} = LocDictionaryEntry,
                storeAndEmptyDictionary(RootTable, SubDictionary, TableName)
            end, TableList);
    
terminate(Reason, S) ->
    io:format("Buffered_logger: other termination: ~p.~n", [Reason]).

% =======================================================================
% ======================= </gen_server functions> =======================
% =======================================================================