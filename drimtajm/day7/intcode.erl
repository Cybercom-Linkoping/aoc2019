%%%-------------------------------------------------------------------
%%% @author Angela Johansson <dreamtime@cybercat>
%%% @copyright (C) 2019, Angela Johansson
%%% @doc
%%%
%%% @end
%%% Created :  7 Dec 2019 by Angela Johansson <dreamtime@cybercat>
%%%-------------------------------------------------------------------
-module(intcode).

-behaviour(gen_statem).

%% API
-export([start_link/2, send_input/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([running/3, waiting_for_input/3]).

-record(data, {machineid      :: atom(),
               program        :: list(),
               memory         :: list(),
               pointer        :: integer(),
               outputreceiver :: atom(),
               inputtarget    :: integer(),
               outputvalue    :: integer(),
               caller         :: pid()}).
-record(opcode, {id       :: integer(),
                 name     :: atom(),
                 string   :: string(),
                 operands :: integer()}).
-record(parameter, {mode     :: atom(),
                    position :: integer(),
                    value    :: integer()}).

-define(OPCODES, [#opcode{id = 1, name = add, string = "ADD", operands = 3},
                  #opcode{id = 2, name = multiply, string = "MULT", operands = 3},
                  #opcode{id = 3, name = input, string = "INPUT", operands = 1},
                  #opcode{id = 4, name = output, string = "OUTPUT", operands = 1},
                  #opcode{id = 5, name = jumpifnotzero, string = "JUMPNZ", operands = 2},
                  #opcode{id = 6, name = jumpifzero, string = "JUMPIZ", operands = 2},
                  #opcode{id = 7, name = lessthan, string = "LESS", operands = 3},
                  #opcode{id = 8, name = equal, string = "EQUAL", operands = 3},
                  #opcode{id = 99, name = halt, string = "HALT", operands = 0}]).
-define(debug(DebugEnabled, FormatString, Args), case DebugEnabled of
                                                     true  -> io:format(FormatString, Args);
                                                     false -> ok
                                                 end).
-define(getopcodebyid(Id), lists:keyfind(Id, #opcode.id, ?OPCODES)).

%%%===================================================================
%%% API
%%%===================================================================

send_input(Receiver, Input) ->
    try gen_statem:call(Receiver, {input, Input}) of
        ok -> ok
    catch
        exit:_Exit -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
start_link(OwnId, Args) ->
    LogFile = lists:concat(["machine_", OwnId, ".log"]),
    gen_statem:start_link({local, OwnId}, ?MODULE, [{id, OwnId} | Args], [{log_to_file, LogFile}]).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  gen_statem:init_result(atom()).
init([{id, Id}, {caller, Caller}, {outputreceiver, Receiver}, {program, InitialProgram}]) ->
    process_flag(trap_exit, true),
    {ok, running, #data{machineid = Id,
                        program = InitialProgram,
                        memory = InitialProgram,
                        pointer = 0,
                        outputreceiver = Receiver,
                        inputtarget = 0,
                        outputvalue = 0,
                        caller = Caller}, [{next_event, internal, run}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function
%% with the name of the current state (StateName)
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
running(internal, run, #data{program = Program, pointer = Pointer, memory = Memory} = Data) ->
    %% upon input, enter state "wait for input"
    %% upon output, repeat state, but do action "output"
    %% upon halt, stop
    %% otherwise, repeat state
    {Opcode, Params} = readop(Program),
    case Opcode#opcode.name of
        halt ->
            Data#data.caller ! {output, Data#data.machineid, Data#data.outputvalue},
            {stop, normal};
        input ->
            {NewProgram, NewPointer} = step(Opcode, Program, Pointer),
            {next_state, waiting_for_input,
             Data#data{program = NewProgram, pointer = NewPointer, inputtarget = hd(Params)},
             [hibernate]};
        output ->
            Output = lookupparameter(hd(Params), Memory),
            case Data#data.outputreceiver of
                no_value -> void;
                Receiver -> ok = send_input(Receiver, Output)
            end,
            {NewProgram, NewPointer} = step(Opcode, Program, Pointer),
            {keep_state, Data#data{program = NewProgram, pointer = NewPointer, outputvalue = Output},
             [{next_event, internal, run}]};
        Jump when (Jump =:= jumpifzero) orelse (Jump =:= jumpifnotzero) ->
            Condition = lookupparameter(hd(Params), Memory),
            {NewProgram, NewPointer} =
                case evaluatejumpcondition(Jump, Condition) of
                    true  -> JumpPointer = lookupparameter(hd(tl(Params)), Memory),
                             {seek(JumpPointer, Memory), JumpPointer};
                    false -> step(Opcode, Program, Pointer)
                end,
            {keep_state, Data#data{program = NewProgram, pointer = NewPointer},
             [{next_event, internal, run}]};
        _Else ->
            [Parameter1, Parameter2, OutParameter] = Params,
            Result = compute(Opcode, Parameter1, Parameter2, Memory),
            {NewProgram0, NewPointer} = step(Opcode, Program, Pointer),
            NewProgram = writemem(NewPointer, NewProgram0, OutParameter#parameter.position, Result),
            NewMemory  = writemem(0, Memory, OutParameter#parameter.position, Result),
            {keep_state, Data#data{program = NewProgram, pointer = NewPointer, memory = NewMemory},
             [{next_event, internal, run}]}
    end;
running({call, _From}, _Msg, _Data) ->
    {keep_state_and_data, [postpone]}.

waiting_for_input({call, From}, {input, InputValue}, #data{program = Program, pointer = Pointer,
                                                           memory = Memory, inputtarget = InputTarget} = Data) ->
    NewProgram = writemem(Pointer,Program, InputTarget#parameter.position, InputValue),
    NewMemory  = writemem(0, Memory, InputTarget#parameter.position, InputValue),
    {next_state, running, Data#data{program = NewProgram, memory = NewMemory}, [{reply, From, ok},
                                                                                {next_event, internal, run}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
                         {ok, NewState :: term(), NewData :: term()} |
                         (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

formatop(#opcode{string = String}) ->
    String.

formatparam(#parameter{mode = Mode, value = Value}) when Mode =:= immediate ->
    list_to_integer(Value);
formatparam(#parameter{mode = Mode, value = Value}) when Mode =:= positional ->
    io_lib:format("$~p", [Value]);
formatparam(_Else) ->
    "".

debugcode(false, _, _, _, _, _, _) ->
    noop;
debugcode(true, Pointer, Opcode, Value, Param1, Param2, Outparam) ->
    OpString = formatop(Opcode),
    Param1String = formatparam(Param1),
    Param2String = formatparam(Param2),
    OutparamString = formatparam(Outparam),
    io:format("~.4p) ~.6s ~.3p | ~.5s ~.5s | to: ~s~n",
              [Pointer, OpString, Value, Param1String, Param2String, OutparamString]).

%%%%=================================================================

readmem(Position, Memory) ->
    lists:nth(Position+1, Memory).

writemem(CurrentPointer, Memory, Position, _Value) when Position < CurrentPointer ->
    Memory;
writemem(CurrentPointer, Memory, Position, Value) ->
    Offset = Position - CurrentPointer,
    {MemoryBefore, MemoryAtPosition} = lists:split(Offset, Memory),
    MemoryBefore ++ [Value | tl(MemoryAtPosition)].

seek(Position, Memory) ->
    lists:nthtail(Position, Memory).

lookupparameter(#parameter{mode = positional, position = Position}, Memory) ->
    readmem(Position, Memory);
lookupparameter(#parameter{mode = immediate, value = Value}, _Memory) ->
    Value.

compute(#opcode{name = Op}, Parameter1, Parameter2, Memory) ->
    Term1 = lookupparameter(Parameter1, Memory),
    Term2 = lookupparameter(Parameter2, Memory),
    case Op of
        add      -> Term1 + Term2;
        multiply -> Term1 * Term2;
        lessthan -> if Term1 < Term2 -> 1;
                       true          -> 0
                    end;
        equal    -> if Term1 =:= Term2 -> 1;
                       true            -> 0
                    end
    end.

readparams(Opcode, Modes, Program) ->
    readparams(Opcode, Modes, Program, []).
readparams(#opcode{operands = 0}, _Modes, _Program, Params) ->
    lists:reverse(Params);
readparams(#opcode{operands = Operands} = Opcode, [Mode | Modes], [Param0 | Program], Params) ->
    Param = case Mode of
                0 -> #parameter{mode = positional, position = Param0};
                1 -> #parameter{mode = immediate, value = Param0}
            end,
    readparams(Opcode#opcode{operands = Operands-1}, Modes, Program, [Param | Params]).

readop([Instruction | Program]) ->
    OpId = Instruction rem 100,
    Mode1 = (Instruction div 100) rem 10,
    Mode2 = (Instruction div 1000) rem 10,
    OutparamMode = (Instruction div 10000) rem 10,
    Opcode = ?getopcodebyid(OpId),
    Modes  = [Mode1, Mode2, OutparamMode],
    Params = readparams(Opcode, Modes, Program),
    {Opcode, Params}.

step(#opcode{operands = Operands}, Program, Pointer) ->
    NewProgram = seek(Operands+1, Program),
    NewPointer = Pointer + Operands + 1,
    {NewProgram, NewPointer}.

evaluatejumpcondition(jumpifzero, Condition) when Condition =:= 0 ->
    true;
evaluatejumpcondition(jumpifnotzero, Condition) when Condition =/= 0 ->
    true;
evaluatejumpcondition(_Jump, _Condition) ->
    false.
