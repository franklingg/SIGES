:- module(utils, [emptyDict/1, promptNumber/2, promptString/2, promptChoice/2, cls/0,
                  waitInput/0, waitInput/1, searchDict/3, getInputData/4, getInputData/2,
                  getYesOrNo/1, getNumber/1, getDate/3, getTime/2, trivial/1, timeNow/1, 
                  promptTest/1, stringBuilder/3, xor/2, yesOrNo/1]).
:- encoding(utf8).

:- use_module("./Handlers/errorHandler.pl").
:- use_module(library(ansi_term)).

cls :- write("\e[H\e[2J").

emptyDict(D):-D=_{}.

searchDict(Dict, Key, Result):-
    string_length(Key, L), L =:= 1,
    sub_atom(Key, 0, 1, _, C1), string_lower(C1, R1), atom_string(K,R1),
    Result=Dict.get(K).

promptNumber(Text, Value) :- promptString(Text, V), atom_number(V, Value).

promptString(Text, Value) :- read_pending_chars(user_input, _, _),
                             write(Text), flush_output(user),
                             read_line_to_string(user_input, Value).


promptChoice(Options, Value) :- promptString(">> ", S),
                                searchDict(Options, S, Value),!;
                                errorHandler:promptError(1),
                                promptChoice(Options,Value).

promptTest(Message):- ansi_format([bold,fg(yellow)], "~w", [Message]).

waitInput:-waitInput("").

waitInput(S):-
    write(S), write("Aperte qualquer tecla para continuar."),get_single_char(_),nl.
    
getInputData(Checker, Value, NextScreens, Next):-
    promptString(">> ", V),
    (searchDict(NextScreens,V,R),nonvar(R),Next=R;
    call(Checker, V),
    Value=V,!;
    getInputData(Checker, Value, NextScreens, Next)).

getInputData(Checker, Value):-
    emptyDict(N),
    getInputData(Checker, Value, N, _).

yesOrNo(String):-
    string_length(String, L),L =:= 1,
    sub_atom(String, 0, 1, _, C), string_lower(C, R),
    (R="s";R="n"),!;
    errorHandler:promptError(5), fail.

getYesOrNo(Answer):-
    getInputData(yesOrNo, A),
    string_lower(A, Ans),
    (Ans="s"->Answer=true,!;Answer=false).

checkNumber(NumberStr):-
    number_string(N, NumberStr),
    N > 0, N =< 300, !;
    errorHandler:promptError(15),fail.

getNumber(Number):-
    getInputData(checkNumber, NumberStr),
    number_string(Number, NumberStr).

checkDate(DateStr):-
    string_length(DateStr,L), L == 10,
    split_string(DateStr, "-", "", [D, M, _]),
    number_string(Day, D), Day >= 1, Day =< 31,
    number_string(Month, M), Month >= 1, Month =< 12,!;
    errorHandler:promptError(16),fail.

getDate(Day,Month,Year):-
    getInputData(checkDate, DateStr),
    split_string(DateStr, "-", "", [D, M, Y]),
    number_string(Day, D), number_string(Month, M),number_string(Year, Y).

checkTime(TimeStr):-
    string_length(TimeStr, L), L = 5,
    split_string(TimeStr, ":", "", [H, M]),
    number_string(Hour, H), Hour >= 0, Hour < 24,
    number_string(Minutes, M), Minutes >= 0, Minutes < 60,!;
    errorHandler:promptError(17),fail.

getTime(Hours, Minutes):-
    getInputData(checkTime, TimeStr),
    split_string(TimeStr, ":", "", [H, M]),
    number_string(Hours, H),number_string(Minutes, M).

trivial(_).

timeNow(D):- get_time(X), stamp_date_time(X, D, 10800).

stringBuilder([], Aux, Aux).
stringBuilder([H|T], Aux, Text) :- %Uma lista de strings, Um auxiliar (iniciar com uma string vazia) e uma variável Text para a resposta.
    string_concat(Aux, H, Intermediate),
    stringBuilder(T, Intermediate, Text).
    
xor(A,B):- (A,not(B)),!;(not(A),B).