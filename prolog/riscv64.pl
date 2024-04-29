:- use_module(library(clpz)).
:- use_module(library(debug)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(lambda)).

% reg(Name, Code, Description)
reg(x0, 0, "zero").
reg(x1, 1, "zero").
reg(x2, 2, "zero").

% instr(Name, Coding, Description)
% Coding can be
% - i(Opcode, Funct3)
instr(addi, i(19, 0), "ADDI adds the sign-extended 12-bit immediate to register rs1. Arithmetic overflow is ignored and the result is simply the low XLEN bits of the result").
instr(slti, i(19, 2), "SLTI (set less than immediate) places the value 1 in register rd if register rs1 is less than the sign-extended immediate when both are treated as signed numbers, else 0 is written to rd. SLTIU is similar but compares the values as unsigned numbers (i.e., the immediate is first sign-extended to XLEN bits then treated as an unsigned number). Note, SLTIU rd, rs1, 1 sets rd to 1 if rs1 equals zero, otherwise sets rd to 0 (assembler pseudoinstruction SEQZ rd, rs).").
instr(sltiu, i(19, 3), Doc) :- instr(slti, _, Doc).
instr(andi, i(19, 7), "ANDI, ORI, XORI are logical operations that perform bitwise AND, OR, and XOR on register rs1 and the sign-extended 12-bit immediate and place the result in rd. Note, XORI rd, rs1, -1 performs a bitwise logical inversion of register rs1 (assembler pseudoinstruction NOT rd, rs).").
instr(ori, i(19, 6), Doc) :- instr(andi, _, Doc).
instr(xori, i(19, 4), Doc) :- instr(andi, _, Doc).

instr_doc_(InstrName) -->
    { instr(InstrName, i(Opcode, Funct3), Doc), format_binary(7, Opcode, OpcodeBin), format_binary(3, Funct3, Funct3Bin) },
    format_("~a~n", [InstrName]),
    format_("IIIIIIIIIIIIAAAAA~sBBBBB~s~n", [Funct3Bin, OpcodeBin]),
    "I = 12-bit immediate\n",
    "A = rs1\n",
    "B = rd\n",
    Doc.

instr_doc(InstrName) :-
    phrase(instr_doc_(InstrName), Text),
    format("~s", [Text]).

all_instr(I) :-
    findall(NameStr, (instr(Name, _, _), atom_chars(Name, NameStr)), I0), sort(I0, I).

format_binary(L, N, S) :-
    length(S, L),    
    append(S0, S1, S),
    maplist(\X^(X='0'), S0),
    phrase(format_("~2r", [N]), S1).

% Instr can be instr(addi, [reg(x1), reg(x2), imm(100)])
% TODO: Add reverse
% TODO: Params validation
instr_binary(Instr, Code) :-
    Instr = instr(Name, Params),
    instr(Name, Coding, _),
    Coding = i(Opcode, Funct3),
    Params = [reg(Dst), reg(Src), imm(Imm)],
    reg(Dst, Rd, _),
    reg(Src, Rs1, _),
    binformat([field(Imm, 31, 20), field(Rs1, 19, 15), field(Funct3, 14, 12), field(Rd, 11, 7), field(Opcode, 6, 0)], Code).

% binformat(Fields, Binary)
% Fields is a list of field(Data, StartingPos, EndingPos)
% TODO: Field validation
% TODO: Add reverse version
binformat([], 0).
binformat([F|Fs], Code) :-
    binformat(Fs, C0),
    F = field(Data, _Start, End),
    Code #= (Data << End) \/ C0.
    
    
