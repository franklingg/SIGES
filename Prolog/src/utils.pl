/** <module> Utils
 * 
 * Módulo que reúne várias operações utilitárias usadas em diferentes partes do sistema,
 * permite trabalhar com dicionários, entradas de usuário, datas/tempo, checagem de valores, etc.
 *
 * @license GPL
 */
:- module(utils, [emptyDict/1, promptNumber/2, promptString/2, promptChoice/2, cls/0,
                  waitInput/0, waitInput/1, searchDict/3, getInputData/4, getInputData/2,
                  getYesOrNo/1, getNumber/1, getDate/3, getTime/2, trivial/1, timeNow/1, 
                  promptTest/1, stringBuilder/3, xor/2, yesOrNo/1]).
:- encoding(utf8).

:- use_module("./Handlers/errorHandler.pl").
:- use_module(library(ansi_term)).

/**
 * cls is det.
 *
 * Limpa o terminal.
 */
cls :- write("\e[H\e[2J").

/**
 * emptyDict(-D: dict) is det.
 *
 * Retorna um dicionário vazio, sem chaves e elementos.
 * @param D Dicionário vazio
 */
emptyDict(D):-D=_{}.

/**
 * searchDict(+Dict:dict, +Key:string, -Result:any) is semidet.
 * 
 * Busca um elemento em um dicionário por sua chave, sendo a chave um único caractere.
 * @param Dict Dicionário a ser buscado
 * @param Key Chave a buscar no dicionário
 * @param Result Resultado da busca
 */
searchDict(Dict, Key, Result):-
    string_length(Key, L), L =:= 1,
    sub_atom(Key, 0, 1, _, C1), string_lower(C1, R1), atom_string(K,R1),
    Result=Dict.get(K).

/**
 * promptNumber(+Text:string, -Value:number) is semidet.
 * 
 * Imprime uma mensagem no terminal e espera um input do usuário, retornando o valor como número.
 * @param Text Texto a ser impresso antes da entrada do usuário
 * @param Value Valor numérico retornado
 */ 
promptNumber(Text, Value) :- promptString(Text, V), atom_number(V, Value).

/**
 * promptString(+Text:string, -Value:string) is det.
 * 
 * Imprime uma mensagem no terminal e espera um input do usuário, retornando o valor inserido.
 * @param Text Texto a ser impresso antes da entrada do usuário
 * @param Value Valor retornado
 */ 
promptString(Text, Value) :- read_pending_chars(user_input, _, _),
                             write(Text), flush_output(user),
                             read_line_to_string(user_input, Value).

/**
 * promptChoice(+Options:dict, -Value:any) is multi.
 * 
 * Recebe um dicionário e espera o usuário entrar com algum valor, então faz a busca no dicionário
 * e retorna o valor encontrado, ou então imprime uma mensagem de erro e recebe um novo valor.
 * @param Options Dicionário com as opções possíveis
 * @param Value Valor encontrado no dicionário
 */ 
promptChoice(Options, Value) :- promptString(">> ", S),
                                searchDict(Options, S, Value),!;
                                errorHandler:promptError(1),
                                promptChoice(Options,Value).

/**
 * promptTest(+Message:string) is det.
 * 
 * Recebe uma mensagem de teste e imprime com a coloração adequada.
 * @param Message Mensagem a ser impressa com uma fonte diferente
 */ 
promptTest(Message):- ansi_format([bold,fg(blue)], "~w", [Message]).

/**
 * waitInput is det.
 * 
 * Espera o usuário digitar algum caractere.
 */ 
waitInput:-waitInput("").

/**
 * waitInput(+S:string) is det. 
 * 
 * Imprime a mensagem fornecida, esperando o usuário digitar algum caractere.
 * @param S Mensagem a ser impressa antes de esperar a entrada
 */
waitInput(S):-
    ansi_format([bold,fg(yellow)], "~w", [S]),
    ansi_format([bold,fg(yellow)], "~w", ["Aperte qualquer tecla para continuar."]),
    get_single_char(_),nl.

/**
 * getInputData(+Checker:compound, -Value: string, NextScreens:dict, Next:string) is multi.
 * 
 * Recebe um dado de entrada do usuário, checando sua validade com um verificador, tendo também
 * a possibilidade de definir a próxima tela do sistema.
 * @param Checker Função que faz a checagem da entrada de usuário
 * @param Value Valor verificado e retornado de entrada
 * @param NextScreens dicionário com as próximas telas possíveis
 * @param Next Próxima tela a ser apresentada ao usuário
 */ 
getInputData(Checker, Value, NextScreens, Next):-
    promptString(">> ", V),
    (searchDict(NextScreens,V,R),nonvar(R),Next=R;
    call(Checker, V),
    Value=V,!;
    getInputData(Checker, Value, NextScreens, Next)).

/* getInputData(+Checker:compound, -Value: string) is multi.
 * 
 * Recebe um dado de entrada do usuário, checando sua validade com um verificador.
 * @param Checker Função que faz a checagem da entrada de usuário
 * @param Value Valor verificado e retornado de entrada
 */
getInputData(Checker, Value):-
    emptyDict(N),
    getInputData(Checker, Value, N, _).

/**
 * yesOrNo(+String:string) is semidet.
 * 
 * Determina se a string recebida funciona como SIM/NÃO, se for um dos caracteres: ['S','N','s','n'].
 * param String String a ser verificada
 */ 
yesOrNo(String):-
    string_length(String, L),L =:= 1,
    sub_atom(String, 0, 1, _, C), string_lower(C, R),
    (R="s";R="n"),!;
    errorHandler:promptError(5), fail.

/**
 * getYesOrNo(-Answer:boolean) is multi.
 * 
 * Recebe entradas do usuário e retorna um valor booleano quando a resposta equivaler a um
 * tipo SIM/NÃO.
 * @param Answer Representação de SIM/NÃO em booleano
 */ 
getYesOrNo(Answer):-
    getInputData(yesOrNo, A),
    string_lower(A, Ans),
    (Ans="s"->Answer=true,!;Answer=false).

/**
 * checkNumber(+NumberStr:string) is semidet.
 *
 * Determina se a string recebida pode funcionar como um numero positivo menor que 300.
 * @param NumberStr String a ser testada
 */ 
checkNumber(NumberStr):-
    number_string(N, NumberStr),
    N > 0, N =< 300, !;
    errorHandler:promptError(15),fail.

/**
 * getNumber(-Number:number) is multi.
 * 
 * Recebe entradas do usuário e retorna um valor numérico quando a resposta 
 * for aprovada pelo checkNumber/1.
 * @param Number Entrada do usuário como número
 */ 
getNumber(Number):-
    getInputData(checkNumber, NumberStr),
    number_string(Number, NumberStr).

/**
 * checkDate(+DateStr:string) is semidet.
 *
 * Determina se a string recebida pode funcionar como uma data válida.
 * @param DateStr String a ser testada
 */ 
checkDate(DateStr):-
    string_length(DateStr,L), L == 10,
    split_string(DateStr, "-", "", [D, M, _]),
    number_string(Day, D), Day >= 1, Day =< 31,
    number_string(Month, M), Month >= 1, Month =< 12,!;
    errorHandler:promptError(16),fail.

/**
 * getDate(-Day:int, -Month:int, -Year:int) is multi.
 * 
 * Recebe entradas do usuário e retorna o dia, mês e ano quando estes forem aprovados
 * pelo checkDate/1.
 * @param Day Dia fornecido pelo usuário
 * @param Month Mês fornecido pelo usuário
 * @param Year Ano fornecido pelo usuário
 */ 
getDate(Day,Month,Year):-
    getInputData(checkDate, DateStr),
    split_string(DateStr, "-", "", [D, M, Y]),
    number_string(Day, D), number_string(Month, M),number_string(Year, Y).

/**
 * checkTime(+TimeStr:string) is semidet.
 *
 * Determina se a string recebida pode funcionar como um horário válido.
 * @param TimeStr String a ser testada
 */ 
checkTime(TimeStr):-
    string_length(TimeStr, L), L = 5,
    split_string(TimeStr, ":", "", [H, M]),
    number_string(Hour, H), Hour >= 0, Hour < 24,
    number_string(Minutes, M), Minutes >= 0, Minutes < 60,!;
    errorHandler:promptError(17),fail.
/**
 * getDate(-Hours:int, -Minutes:int) is multi.
 * 
 * Recebe entradas do usuário e retorna a horas e os minutos quando estes forem aprovados
 * pelo checkTime/1.
 * @param Hours Valor de horas fornecido pelo usuário
 * @param Minutes Valor de minutos fornecido pelo usuário
 */ 
getTime(Hours, Minutes):-
    getInputData(checkTime, TimeStr),
    split_string(TimeStr, ":", "", [H, M]),
    number_string(Hours, H),number_string(Minutes, M).

/**
 * trivial(_:any) is det. 
 * 
 * Verificador que sempre retorna verdadeiro, independete de quais valores receba.
 */
trivial(_).

/**
 * timeNow(-D:compound) is det.
 *
 * Retorna o horário atual no formato apropriado de date_time [Y,M,D,H,M,S,TIMEZONE,-,-].
 * @param D Horário atual como date/9
 */
timeNow(D):- get_time(X), stamp_date_time(X, D, 10800).

/**
 * stringBuilder(+List:list, +Aux:string, Text: string) is det.
 * 
 * Constrói uma string a partir de uma lista de strings, concatenando-as.
 * @param List Lista de strings a ser concatenada
 * @param Aux Ponto de partida da construção (normalmente string vazia)
 * @param Text Resultado da operação de construção
 */ 
stringBuilder([], Aux, Aux).
stringBuilder([H|T], Aux, Text) :- %Uma lista de strings, Um auxiliar (iniciar com uma string vazia) e uma variável Text para a resposta.
    string_concat(Aux, H, Intermediate),
    stringBuilder(T, Intermediate, Text).

/**
 * xor(+A:boolean, +B:boolean) is semidet.
 * 
 * Realiza a operação lógica de XOR, isto é, sucede se os termos tem valores diferentes (TF, FT) e
 * falha caso contrário (TT, FF).
 */ 
xor(A,B):- (A,not(B)),!;(not(A),B).