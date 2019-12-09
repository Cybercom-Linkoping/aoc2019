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
               memory         :: list(),
               pointer        :: integer(),
               relativebase   :: integer(),
               outputreceiver :: atom(),
               inputparameter :: tuple(),
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
                  #opcode{id = 9, name = modifybase, string = "MBASE", operands = 1},
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
                        memory = initializememory(InitialProgram),
                        pointer = 0,
                        relativebase = 0,
                        outputreceiver = Receiver,
                        inputparameter = #parameter{},
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
running(internal, run, #data{pointer = Pointer, memory = Memory,
                             relativebase = RelativeBase} = Data) ->
    %% upon input, enter state "wait for input"
    %% upon output, repeat state, but do action "output"
    %% upon halt, stop
    %% otherwise, repeat state
    {Opcode, Params} = readop(Memory, Pointer),
    case Opcode#opcode.name of
        halt ->
            %%io:format("Final pointer: ~p~nFinal memory: ~p~n",
            %%          [Pointer, lists:sort(dict:to_list(Memory))]),
            Data#data.caller ! {output, Data#data.machineid,
                                Data#data.outputvalue},
            {stop, normal};
        input ->
            NewPointer = step(Opcode, Pointer),
            {next_state, waiting_for_input,
             Data#data{pointer = NewPointer, inputparameter = hd(Params)},
             [hibernate]};
        output ->
            Output = lookupparameter(hd(Params), Memory, RelativeBase),
            case Data#data.outputreceiver of
                no_value -> void;
                Receiver -> ok = send_input(Receiver, Output)
            end,
            NewPointer = step(Opcode, Pointer),
            {keep_state, Data#data{pointer = NewPointer, outputvalue = Output},
             [{next_event, internal, run}]};
        modifybase ->
            Result = compute(#opcode{name = add},
                             #parameter{mode = immediate,
                                        value = Data#data.relativebase},
                             hd(Params), Memory, RelativeBase),
            NewPointer = step(Opcode, Pointer),
            {keep_state, Data#data{pointer = NewPointer, relativebase = Result},
             [{next_event, internal, run}]};
        Jump when (Jump =:= jumpifzero) orelse (Jump =:= jumpifnotzero) ->
            Condition = lookupparameter(hd(Params), Memory, RelativeBase),
            NewPointer =
                case evaluatejumpcondition(Jump, Condition) of
                    true  -> lookupparameter(hd(tl(Params)), Memory, RelativeBase);
                    false -> step(Opcode, Pointer)
                end,
            {keep_state, Data#data{pointer = NewPointer},
             [{next_event, internal, run}]};
        _Else ->
            [Parameter1, Parameter2, OutParameter] = Params,
            Result = compute(Opcode, Parameter1, Parameter2, Memory, RelativeBase),
            NewPointer = step(Opcode, Pointer),
            NewMemory = writemem(Memory, OutParameter, Result, RelativeBase),
            {keep_state, Data#data{pointer = NewPointer, memory = NewMemory},
             [{next_event, internal, run}]}
    end;
running({call, _From}, _Msg, _Data) ->
    {keep_state_and_data, [postpone]}.

waiting_for_input({call, From}, {input, InputValue},
                  #data{memory = Memory, inputparameter = InputParameter,
                        relativebase = RelativeBase} = Data) ->
    NewMemory = writemem(Memory, InputParameter, InputValue, RelativeBase),
    {next_state, running, Data#data{memory = NewMemory},
     [{reply, From, ok}, {next_event, internal, run}]}.

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
    case dict:find(Position, Memory) of
        {ok, Value} -> Value;
        error       -> 0
    end.

writemem(Memory, Parameter, Value, RelativeBase) ->
    Position = getaddress(Parameter, RelativeBase),
    dict:store(Position, Value, Memory).

getaddress(#parameter{mode = positional, position = Position}, _RelativeBase) ->
    Position;
getaddress(#parameter{mode = relative, position = Relative}, RelativeBase) ->
    RelativeBase + Relative;
getaddress(Parameter, RelativeBase) ->
    io:format("param: ~p, relbase: ~p~n", [Parameter, RelativeBase]),
    throw("ADDRESS ERROR").

lookupparameter(#parameter{mode = immediate, value = Value}, 
                _Memory, _RelativeBase) ->
    Value;
lookupparameter(Parameter, Memory, RelativeBase) ->
    Position = getaddress(Parameter, RelativeBase),
    readmem(Position, Memory).

compute(#opcode{name = Op}, Parameter1, Parameter2, Memory, RelativeBase) ->
    Term1 = lookupparameter(Parameter1, Memory, RelativeBase),
    Term2 = lookupparameter(Parameter2, Memory, RelativeBase),
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

readparams(Opcode, Modes, Memory, Pointer) ->
    readparams(Opcode, Modes, Memory, Pointer+1, []).
readparams(#opcode{operands = 0}, _Modes, _Memory, _Pointer, Params) ->
    lists:reverse(Params);
readparams(#opcode{operands = Operands} = Opcode, [Mode | Modes],
           Memory, Pointer, Params) ->
    Param0 = readmem(Pointer, Memory),
    Param = case Mode of
                0 -> #parameter{mode = positional, position = Param0};
                1 -> #parameter{mode = immediate, value = Param0};
                2 -> #parameter{mode = relative, position = Param0}
            end,
    readparams(Opcode#opcode{operands = Operands-1}, Modes, 
               Memory, Pointer+1, [Param | Params]).

readop(Memory, InstructionPointer) ->
    Instruction = readmem(InstructionPointer, Memory),
    OpId = Instruction rem 100,
    Mode1 = (Instruction div 100) rem 10,
    Mode2 = (Instruction div 1000) rem 10,
    OutparamMode = (Instruction div 10000) rem 10,
    Opcode = ?getopcodebyid(OpId),
    Modes  = [Mode1, Mode2, OutparamMode],
    Params = readparams(Opcode, Modes, Memory, InstructionPointer),
    {Opcode, Params}.

step(#opcode{operands = Operands}, Pointer) ->
    Pointer + Operands + 1.

evaluatejumpcondition(jumpifzero, Condition) when Condition =:= 0 ->
    true;
evaluatejumpcondition(jumpifnotzero, Condition) when Condition =/= 0 ->
    true;
evaluatejumpcondition(_Jump, _Condition) ->
    false.

initializememory(InitialProgram) ->
    Addresses = lists:seq(0, length(InitialProgram)-1),
    dict:from_list(lists:zip(Addresses, InitialProgram)).
