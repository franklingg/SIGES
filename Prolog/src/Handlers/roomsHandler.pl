/** <module> RoomsHandler
* Description : Módulo contendo as operações básicas para manipulação de salas no sistema SIGES.
*/
:- module(roomsHandler, [createRoom/5, getRoom/2, deleteRoom/1, makeReservation/5,
                         deleteReservation/3, findReservation/3, editReservation/5,
                         searchRoomsCapacity/2, searchRoomsCategory/2, searchRoomsRequester/2,
                         searchRoomsResources/2, searchRoomsTime/3, cleanAllReservations/0,
                         printCategories/0, printResources/0, showRoom/2, createReportForTheRoom/3,
                         createReportForTheDay/2,checkNewRoomCode/1]).
:- encoding(utf8).
:- use_module("./../utils.pl").
:- use_module("./dataHandler.pl").

/*
* Função que, dadas todas as informações de uma sala, a cria e a persiste no banco de dados.
*/
createRoom(CodeStr, Resources, Category, Capacity, Localization) :-
    string_upper(CodeStr, Code),
    dataHandler:notExistingRoom(Code),
    R = room(Code, [], Resources, Category, Capacity, Localization),
    dataHandler:saveRoom(R), !;
    errorHandler:promptError(9), fail.

/*
* Função que considerará uma String, e retornará a sala cadastrada no sistema com o código igual a esta String, caso exista.
*/
getRoom(CodeStr, R) :-
    string_upper(CodeStr, Code),
    dataHandler:readRooms,
    bagof(_, dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization), List),
    dataHandler:cleanRooms,
    List\=[],
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),!;
    errorHandler:promptError(10), fail.

/*
* Função que considera uma String, e caso ela corresponda ao código de uma das salas, a função eliminará a sala equivalente do sistema, retornando se a remoção pôde ser feita.
*/
deleteRoom(CodeStr):-
    string_upper(CodeStr, Code),
    dataHandler:readRooms, 
    findall(_,dataHandler:room(Code, _, _, _, _, _),List),List\=[],
    retract(dataHandler:room(Code, _, _, _, _, _)),
    dataHandler:writeRooms,!;
    errorHandler:promptError(10), fail.

/*
* Função que verifica se esta sala estará livre neste horário.
*/
isFree([], _, _).

isFree([H|T], StartTime, FinishTime):-
    H = reservation(_, _, StartTimeRoom, FinishTimeRoom),
    utils:xor((StartTimeRoom @>= FinishTime), (FinishTimeRoom @=< StartTime )),
    isFree(T, StartTime, FinishTime).

/*
* Função que criará uma reserva em uma das salas, a partir do código da mesma, do nome do responsável pela reserva, e das tuplas especificando data e horário de finalização do evento.
*/
makeReservation(CodeStr, UserName, Description, StartTime, FinishTime) :-
    getRoom(CodeStr, R),
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),
    isFree(Schedule, StartTime, FinishTime),
    NewReservation = reservation(UserName, Description, StartTime, FinishTime),
    append(Schedule,[NewReservation],NewSchedule),
    NewRoom = room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(CodeStr),
    dataHandler:saveRoom(NewRoom),!;
    errorHandler:promptError(11), fail.

/*
* Função que checa se existe uma reserva em uma das salas.
*/
existsReservation([], _, _) :- false.

existsReservation([H|T], UserName, StartTime) :-
    H = reservation(UserName, _, StartTime, _),!;
    existsReservation(T, UserName, StartTime).

/*
* Função auxiliar que, dada uma tupla com um Integer para o ano e um Int para mes, dia, hora e minuto, respectivamente, cria um LocalTime.
*/
matchTime(StartTime, Res) :-
    Res = reservation(_, _, StartTime, _).

/*
* Esta função deletará uma reserva identificada pelo código da sala e nome do responsável, e pela horário de início do evento.
*/
deleteReservation(CodeStr, UserName, StartTime) :-
    getRoom(CodeStr, R),
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),
    existsReservation(Schedule, UserName, StartTime),
    
    exclude(matchTime(StartTime), Schedule, NewSchedule),
    NewRoom = room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(CodeStr),
    dataHandler:saveRoom(NewRoom), !;
    errorHandler:promptError(12), fail.

/*
* Dada uma sala, identificada pelo seu código, um horário, esta função retornará a reserva com os dados equivalentes.
*/
findReservation(CodeStr, StartTime, Res) :-
    getRoom(CodeStr, R),
    R = room(_, Schedule, _, _, _, _),
    include(matchTime(StartTime), Schedule, Matching), Matching\=[],
    Matching = [Res|_].

/*
* Função que alterará o horário de uma reserva, identificada pelo código de sua sala, pelo seu responsável e horário de início). Caso não seja possível fazer a alteração, nada será feito. A função retornará um valor indicando se foi possível fazer a operação.
*/
editReservation(CodeStr, UserName, CurrentStartTime, NewStartTime, NewFinishTime) :-
    getRoom(CodeStr, R),
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),
    isFree(Schedule, NewStartTime, NewFinishTime),
    findReservation(CodeStr, CurrentStartTime, Res),
    Res = reservation(Requester, Description, _, _),
    Requester = UserName,
    NewReservation = reservation(Requester, Description, NewStartTime, NewFinishTime),
    exclude(matchTime(CurrentStartTime), Schedule, DeletedSchedule),
    append(DeletedSchedule, [NewReservation], NewSchedule),
    NewRoom = room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(CodeStr),
    dataHandler:saveRoom(NewRoom),!;
    errorHandler:promptError(13), fail.

/*
* Função que retorna se o horário de final do evento já passou.
*/
hasPassed(TimeNow, Res) :-
    Res = reservation(_, _, _, FinishTime),
    TimeNow @>= FinishTime.

/*
* Dado um TimeNow para comparação, esta função irá retirar de uma determinada sala as reservas finalizadas antes deste horário.
*/
cleanReservations(TimeNow, R) :-
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),
    exclude(hasPassed(TimeNow), Schedule, NewSchedule),
    NewRoom = room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(Code),
    dataHandler:saveRoom(NewRoom).

/*
* Função que removerá de todas as salas as reservas cujo horário de final do evento já passou.
*/
cleanAllReservationsAux([], _).
cleanAllReservationsAux([H|T], TimeNow) :-
    cleanReservations(TimeNow, H),
    cleanAllReservationsAux(T, TimeNow).

/*
* Função que removerá de todas as salas as reservas cujo horário de final do evento já passou.
*/
cleanAllReservations :-
    fetchRooms(Rooms),
    timeNow(TimeNow),
    cleanAllReservationsAux(Rooms, TimeNow).

/*
* Função que filtra todas as salas de acordo com a sua categoria.
*/
filterCategory([], _, Aux, Aux).
filterCategory([H|T], Category, Aux, List):-
    H = room(_, _, _, Category, _, _),
    append(Aux, [H], Intermediate), filterCategory(T, Category, Intermediate, List), !;
    filterCategory(T, Category, Aux, List).

/*
* Com uma categoria especificada, esta função verificará o sistema e retornará a lista contendo todas as salas desta categoria.
*/
searchRoomsCategory(Category, Rooms) :-
    fetchRooms(Salas),
    filterCategory(Salas, Category, [], Rooms).

/*
* Função que filtra todas as salas de acordo com a sua capacidade.
*/
filterCapacity([], _, Aux, Aux).
filterCapacity([H|T], Capacity, Aux, List):-
    H = room(_, _, _, _, Cap, _),
    Cap >= Capacity,
    append(Aux, [H], Intermediate), filterCapacity(T, Capacity, Intermediate, List), !;
    filterCapacity(T, Capacity, Aux, List).

/*
* Com uma capacidade especificada, esta função verificará o sistema e retornará a lista contendo todas as salas com esta capacidade ou mais.
*/
searchRoomsCapacity(Capacity, Rooms) :-
    fetchRooms(Salas),
    filterCapacity(Salas, Capacity, [], Rooms).

/*
* Função que monta todas as salas.
*/
mountRooms([], Aux, Aux).

mountRooms([H|T], Aux, Rooms) :-
    H = [Code, Schedule, Resources, Category, Capacity, Localization],
    Room = room(Code, Schedule, Resources, Category, Capacity, Localization),
    append(Aux, [Room], Intermediate),
    mountRooms(T, Intermediate, Rooms).    

/*
* Função que busca todas as salas.
*/
fetchRooms(Rooms) :-
    dataHandler:readRooms,
    findall([Code, Schedule, Resources, Category, Capacity, Localization], dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization), List),
    mountRooms(List, [], Rooms),
    dataHandler:cleanRooms.

/*
* Função que filtra todas as salas de acordo com seu horário.
*/
filterTime([], _, _, Aux, Aux).
filterTime([H|T], StartTime, FinishTime, Aux, List):-
    H = room(_, Schedule, _, _, _, _),
    isFree(Schedule, StartTime, FinishTime),
    append(Aux, [H], Intermediate), filterTime(T, StartTime, FinishTime, Intermediate, List), !;
    filterTime(T, StartTime, FinishTime, Aux, List).

/*
* Com um horário de início e fim especificado em forma de tupla, esta função verificará o sistema e retornará a lista contendo todas as salas que estejam livres neste horário.
*/
searchRoomsTime(StartTime, FinishTime, Rooms):-
    fetchRooms(Salas),
    filterTime(Salas, StartTime, FinishTime, [], Rooms).

/*
* Função que confere a disponibilidade dos recursos nas salas.
*/
resourceIsEnough([], _) :- fail.
resourceIsEnough([H|T], Res) :-
    H = res(Kind, Amount),
    Res = res(RequiredKind, RequiredAmount),
    Kind = RequiredKind,
    Amount >= RequiredAmount, !;
    resourceIsEnough(T, Res).

/*
* Função que confere se a sala tem a disponibilidade dos recursos solicitados.
*/
containsAllResources(_, []).
containsAllResources(Resources, [H|T]) :-
    resourceIsEnough(Resources, H),
    containsAllResources(Resources, T),!;
    fail.

/*
* Função que filtra todas as salas de acordo com seus recursos.
*/
filterResources([], _, Aux, Aux).
filterResources([H|T], RequiredResources, Aux, List):-
    H = room(_, _, Resources, _, _, _),
    containsAllResources(Resources, RequiredResources),
    append(Aux, [H], Intermediate), filterResources(T, RequiredResources, Intermediate, List), !;
    filterResources(T, RequiredResources, Aux, List).

/* 
* Com uma lista de recursos especificada, esta função verificará o sistema e retornará a lista contendo todas as salas que supram esta demanda.
*/
searchRoomsResources(RequiredResources, Rooms) :-
    fetchRooms(Salas),
    filterResources(Salas, RequiredResources, [], Rooms).

/*
* Função que solicita os filtros.
*/
filterRequester([], _, Aux, Aux).
filterRequester([H|T], Requester, Aux, List):-
    H = room(_, Schedule, _, _, _, _),
    wasReservedBy(Requester, Schedule),
    append(Aux, [H], Intermediate), filterRequester(T, Requester, Intermediate, List), !;
    filterRequester(T, Requester, Aux, List).

/*
* Função que verifica se existe alguma solicitação.
*/
hasRequester(UserName, Reservation) :-
    Reservation = reservation(UserName, _, _, _).

/*
* Função que verifica quem realizou a solicitação.
*/
wasReservedBy(UserName, Schedule) :-
    include(hasRequester(UserName), Schedule, List),
    List \= [].

/* 
* Função que verificará o sistema e retornará a lista contendo todas as salas que um determinado usuario solicitou.
*/
searchRoomsRequester(UserName, Rooms) :-
    fetchRooms(Salas),
    filterRequester(Salas, UserName, [], Rooms).

/*
* Função que produzirá um texto contendo a lista de todas as categorias de sala suportadas pelo sistema.
*/
printCategories:-
    writeln("Qual categoria você deseja escolher?\n\c
            [L]aboratório\n\c
            [A]uditório\n\c
            [S]ala de aula\n\c
            [E]scritório\n\c
            [D]epósito").

/* 
* Função que produzirá um texto listando todos os recursos oferecidos pelas salas do sistema.
*/
printResources:-
    writeln("Qual recurso você deseja escolher?\n\c
            [P]rojetor\n\c
            [M]icroscópio\n\c
            [B]irô\n\c
            [C]omputador\n\c
            [Q]uadro\n\c
            [A]r condicionado").

/*
* Função que mostra todos os recursos oferecidos pelas salas do sistema.
*/
showResList([], Aux, Aux).
showResList([H|T], Aux, Text) :-
    H = res(dataHandler:resourceKind(R), Amount),
    dataHandler:resourceKind(R, Name),
    number_string(Amount, StrAmount),

    string_concat("\t", Name, Tabbed),
    string_concat(Tabbed, ": ", Begin),
    string_concat(Begin, StrAmount, Full),
    string_concat(Full, "\n", Line),

    string_concat(Aux, Line, Intermediate),
    showResList(T, Intermediate, Text).

/*
* Função que mostra as informações da sala do sistema.
*/
showRoom(Room, Text) :-
    Room = room(Code, _, Resources, Category, Capacity, Localization),
    Category = dataHandler:category(Cat),
    dataHandler:category(Cat, RoomCat),
    number_string(Capacity, Cap),
    showResList(Resources, "", ResList),
    
    string_concat("\nDados da sala ", Code, L1),
    string_concat("Categoria: ", RoomCat, L2),
    string_concat("Capacidade: ", Cap, L3),
    string_concat("Localização: ", Localization, L4),
    string_concat("Recursos: \n", ResList, L5),

    utils:stringBuilder([L1, "\n", L2, "\n", L3, "\n", L4, "\n", L5], "", Text).

/*
* Função que mostra a data.
*/
showDate(Date, Text) :-
    Date = date(Y,M,D,H,Mn,S,_,_,_),
    number_string(Y, Year),
    number_string(M, Month),
    number_string(D, Day),
    number_string(H, Hour),
    number_string(Mn, Minute),
    number_string(S, Second),
    
    utils:stringBuilder([Day, "/", Month, "/", Year, " ", Hour, ":", Minute, ":", Second], "", Text).

/*
* Função que mostra as informações da reserva da sala do sistema.
*/
showReservation(Reservation, Text) :-
    Reservation = reservation(Requester, Description, StartTime, FinishTime),
    showDate(StartTime, Start),
    showDate(FinishTime, Finish),  

    L1 = "Dados da reserva: ",
    string_concat("Início: ", Start, L2),
    string_concat("Fim: ", Finish, L3),
    string_concat("Responsável: ", Requester, L4),
    string_concat("Motivo: ", Description, L5),

    utils:stringBuilder([L1, "\n", L2, "\n", L3, "\n", L4, "\n", L5], "", Text).

/*
* Função que mostra todos as reservas das salas do sistema.
*/
showReservationList([], Aux, Aux).
showReservationList([H|T], Aux, Text) :-
    showReservation(H, Str),
    string_concat(Str, "\n-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n\n", Part),
    string_concat(Aux, Part, Intermediate),
    showReservationList(T, Intermediate, Text).

/*
* Função que filtra todas as reservas das salas.
*/
filterReservations([], _, Aux, Aux).
filterReservations([H|T], Day, Aux, List) :- %Day é um date/3, diferente do Start e FinishTime, que são date/9
    H = reservation(_, _, StartTime, _),
    StartTime = date(Y,M,D,_,_,_,_,_,_),
    ResDay = date(Y, M, D),

    (ResDay = Day,
    append(Aux, [H], Intermediate),
    !;
    Intermediate = Aux),

    filterReservations(T, Day, Intermediate, List).

/*
* Função que mostra o dia da reserva.
*/
showDay(Date, Text) :-
    Date = date(Y,M,D),
    number_string(Y, Year),
    number_string(M, Month),
    number_string(D, Day),
    
    utils:stringBuilder([Day, "/", Month, "/", Year], "", Text).

/*
* Dado um dia, esta função criará um relatório em texto com todas as reservas que esta sala tem para o dia especificado.
*/
createReportForTheRoom(Day, Room, Text):-
    Room = room(Code, Schedule, _, _, _, _),
    showDay(Day, D),
    filterReservations(Schedule, Day, [], Reservations),
    string_concat("\nRelatório de ocupação para a sala ", Code, Str1),
    string_concat(Str1, " no dia: ", Str2),
    string_concat(Str2, D, Str3),
    string_concat(Str3, ":\n\c", L1),

    showReservationList(Reservations, "", L2),
    (L2="", string_concat(L1, "Sem reservas para mostrar\n", Text),!;
    string_concat(L1,L2,Text)).

/*
* Função que criará um relatório.
*/
createReports(_, [], Aux, Aux).
createReports(Day, [H|T], Aux, Text):-
    createReportForTheRoom(Day, H, Line),
    string_concat(Aux, Line, Intermediate),

    createReports(Day, T, Intermediate, Text).

/*
* Dado um dia representado em uma tupla, esta função criará um relatório em texto para todas as reservas de todas as salas para o dia especificado.
*/
createReportForTheDay(Day, Text) :-
    fetchRooms(Rooms),
    createReports(Day, Rooms, "", Text).


checkNewRoomCode(CodeStr):-
    string_upper(CodeStr, Code),
    dataHandler:readRooms,
    findall(_, dataHandler:room(Code, _, _, _, _, _), List),
    dataHandler:cleanRooms,
    List=[],!;
    errorHandler:promptError(9),fail.

checkRoomCode(CodeStr):-
    getRoom(CodeStr, _).