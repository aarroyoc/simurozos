:- use_module(library(clpz)).
:- use_module(library(debug)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).

% reg(Name, Code, Description)
reg(x0, 0, "zero").
reg(x1, 1, "zero").
reg(x2, 2, "zero").

% instr(Name, Coding, Description)
% Coding can be
% - i(Opcode, Funct3)
instr(addi, i(19, 0), "ADDI adds the sign-extended 12-bit immediate to register rs1. Arithmetic overflow is ignored and the result is simply the low XLEN bits of the result").

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

format_binary(0, _, "").
format_binary(N, Code, Bin) :-
    B #= Code mod 2,
    C0 #= Code div 2,
    N0 #= N - 1,
    format_binary(N0, C0, B0),
    number_chars(B, BStr),
    append(B0, BStr, Bin).

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
    
    
