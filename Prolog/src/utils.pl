:- module(utils, [emptyDict/1, promptNumber/2, promptString/2, promptChoice/2, cls/0,
                  searchDict/3, getInputData/4, yesOrNo/1, timeNow/1, promptTest/1, 
                  stringBuilder/3, xor/2]).
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
                                searchDict(Options, S, Value);
                                errorHandler:promptError(1),
                                promptChoice(Options,Value).

promptTest(Message):- ansi_format([bold,fg(yellow)], "~w", [Message]).
    
getInputData(Checker, Value, NextScreens, Next):-
    promptString(">> ", V),
    (searchDict(NextScreens,V,R),nonvar(R),Next=R;
    call(Checker, V),
    Value=V,!;
    getInputData(Checker, Value, NextScreens, Next)).


yesOrNo(String):-
    string_length(String, L),L =:= 1,
    sub_atom(String, 0, 1, _, C), string_lower(C, R),
    (R="s";R="n"),!;
    errorHandler:promptError(5), fail.


timeNow(D):- get_time(X), stamp_date_time(X, D, 10800).

stringBuilder([], Aux, Aux).
stringBuilder([H|T], Aux, Text) :- %Uma lista de strings, Um auxiliar (iniciar com uma string vazia) e uma variável Text para a resposta.
    string_concat(Aux, H, Intermediate),
    stringBuilder(T, Intermediate, Text).
    
xor(A,B):- (A,not(B)),!;(not(A),B).