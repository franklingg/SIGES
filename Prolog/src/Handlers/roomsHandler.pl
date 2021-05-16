:- module(roomsHandler, [createRoom/5, getRoom/2, deleteRoom/1, makeReservation/5,
                         deleteReservation/3, findReservation/3, editReservation/5,
                         searchRoomsCapacity/2, searchRoomsCategory/2, searchRoomsRequester/2,
                         searchRoomsResources/2, searchRoomsTime/3, cleanAllReservations/0,
                         printCategories/0, printResources/0, showRoom/2, createReportForTheRoom/3,
                         createReportForTheDay/2,checkNewRoomCode/1]).
:- encoding(utf8).
:- use_module("./../utils.pl").
:- use_module("./dataHandler.pl").

createRoom(CodeStr, Resources, Category, Capacity, Localization) :-
    string_upper(CodeStr, Code),
    dataHandler:notExistingRoom(Code),
    R = room(Code, [], Resources, Category, Capacity, Localization),
    dataHandler:saveRoom(R), !;
    errorHandler:promptError(9), fail.

getRoom(CodeStr, R) :-
    string_upper(CodeStr, Code),
    dataHandler:readRooms,
    bagof(_, dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization), List),
    dataHandler:cleanRooms,
    List\=[],
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),!;
    errorHandler:promptError(10), fail.

deleteRoom(CodeStr):-
    string_upper(CodeStr, Code),
    dataHandler:readRooms, 
    findall(_,dataHandler:room(Code, _, _, _, _, _),List),List\=[],
    retract(dataHandler:room(Code, _, _, _, _, _)),
    dataHandler:writeRooms,!;
    errorHandler:promptError(10), fail.

isFree([], _, _).

isFree([H|T], StartTime, FinishTime):-
    H = reservation(_, _, StartTimeRoom, FinishTimeRoom),
    utils:xor((StartTimeRoom @>= FinishTime), (FinishTimeRoom @=< StartTime )),
    isFree(T, StartTime, FinishTime).

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

existsReservation([], _, _) :- false.

existsReservation([H|T], UserName, StartTime) :-
    H = reservation(UserName, _, StartTime, _),!;
    existsReservation(T, UserName, StartTime).

matchTime(StartTime, Res) :-
    Res = reservation(_, _, StartTime, _).

deleteReservation(CodeStr, UserName, StartTime) :-
    getRoom(CodeStr, R),
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),
    existsReservation(Schedule, UserName, StartTime),
    
    exclude(matchTime(StartTime), Schedule, NewSchedule),
    NewRoom = room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(CodeStr),
    dataHandler:saveRoom(NewRoom), !;
    errorHandler:promptError(12), fail.
    
findReservation(CodeStr, StartTime, Res) :-
    getRoom(CodeStr, R),
    R = room(_, Schedule, _, _, _, _),
    include(matchTime(StartTime), Schedule, Matching), Matching\=[],
    Matching = [Res|_].

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

hasPassed(TimeNow, Res) :-
    Res = reservation(_, _, _, FinishTime),
    TimeNow @>= FinishTime.

cleanReservations(TimeNow, R) :-
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),
    exclude(hasPassed(TimeNow), Schedule, NewSchedule),
    NewRoom = room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(Code),
    dataHandler:saveRoom(NewRoom).

cleanAllReservationsAux([], _).
cleanAllReservationsAux([H|T], TimeNow) :-
    cleanReservations(TimeNow, H),
    cleanAllReservationsAux(T, TimeNow).

cleanAllReservations :-
    fetchRooms(Rooms),
    timeNow(TimeNow),
    cleanAllReservationsAux(Rooms, TimeNow).

filterCategory([], _, Aux, Aux).
filterCategory([H|T], Category, Aux, List):-
    H = room(_, _, _, Category, _, _),
    append(Aux, [H], Intermediate), filterCategory(T, Category, Intermediate, List), !;
    filterCategory(T, Category, Aux, List).

searchRoomsCategory(Category, Rooms) :-
    fetchRooms(Salas),
    filterCategory(Salas, Category, [], Rooms).

filterCapacity([], _, Aux, Aux).
filterCapacity([H|T], Capacity, Aux, List):-
    H = room(_, _, _, _, Cap, _),
    Cap >= Capacity,
    append(Aux, [H], Intermediate), filterCapacity(T, Capacity, Intermediate, List), !;
    filterCapacity(T, Capacity, Aux, List).

searchRoomsCapacity(Capacity, Rooms) :-
    fetchRooms(Salas),
    filterCapacity(Salas, Capacity, [], Rooms).

mountRooms([], Aux, Aux).

mountRooms([H|T], Aux, Rooms) :-
    H = [Code, Schedule, Resources, Category, Capacity, Localization],
    Room = room(Code, Schedule, Resources, Category, Capacity, Localization),
    append(Aux, [Room], Intermediate),
    mountRooms(T, Intermediate, Rooms).    

fetchRooms(Rooms) :-
    dataHandler:readRooms,
    findall([Code, Schedule, Resources, Category, Capacity, Localization], dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization), List),
    mountRooms(List, [], Rooms),
    dataHandler:cleanRooms.

filterTime([], _, _, Aux, Aux).
filterTime([H|T], StartTime, FinishTime, Aux, List):-
    H = room(_, Schedule, _, _, _, _),
    isFree(Schedule, StartTime, FinishTime),
    append(Aux, [H], Intermediate), filterTime(T, StartTime, FinishTime, Intermediate, List), !;
    filterTime(T, StartTime, FinishTime, Aux, List).

searchRoomsTime(StartTime, FinishTime, Rooms):-
    fetchRooms(Salas),
    filterTime(Salas, StartTime, FinishTime, [], Rooms).

resourceIsEnough([], _) :- fail.
resourceIsEnough([H|T], Res) :-
    H = res(Kind, Amount),
    Res = res(RequiredKind, RequiredAmount),
    Kind = RequiredKind,
    Amount >= RequiredAmount, !;
    resourceIsEnough(T, Res).

containsAllResources(_, []).
containsAllResources(Resources, [H|T]) :-
    resourceIsEnough(Resources, H),
    containsAllResources(Resources, T),!;
    fail.

filterResources([], _, Aux, Aux).
filterResources([H|T], RequiredResources, Aux, List):-
    H = room(_, _, Resources, _, _, _),
    containsAllResources(Resources, RequiredResources),
    append(Aux, [H], Intermediate), filterResources(T, RequiredResources, Intermediate, List), !;
    filterResources(T, RequiredResources, Aux, List).

searchRoomsResources(RequiredResources, Rooms) :-
    fetchRooms(Salas),
    filterResources(Salas, RequiredResources, [], Rooms).

filterRequester([], _, Aux, Aux).
filterRequester([H|T], Requester, Aux, List):-
    H = room(_, Schedule, _, _, _, _),
    wasReservedBy(Requester, Schedule),
    append(Aux, [H], Intermediate), filterRequester(T, Requester, Intermediate, List), !;
    filterRequester(T, Requester, Aux, List).

hasRequester(UserName, Reservation) :-
    Reservation = reservation(UserName, _, _, _).

wasReservedBy(UserName, Schedule) :-
    include(hasRequester(UserName), Schedule, List),
    List \= [].

searchRoomsRequester(UserName, Rooms) :-
    fetchRooms(Salas),
    filterRequester(Salas, UserName, [], Rooms).
    
printCategories:-
    writeln("Qual categoria você deseja escolher?\n\c
            [L]aboratório\n\c
            [A]uditório\n\c
            [S]ala de aula\n\c
            [E]scritório\n\c
            [D]epósito").

printResources:-
    writeln("Qual recurso você deseja escolher?\n\c
            [P]rojetor\n\c
            [M]icroscópio\n\c
            [B]irô\n\c
            [C]omputador\n\c
            [Q]uadro\n\c
            [A]r condicionado").

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

showDate(Date, Text) :-
    Date = date(Y,M,D,H,Mn,S,_,_,_),
    number_string(Y, Year),
    number_string(M, Month),
    number_string(D, Day),
    number_string(H, Hour),
    number_string(Mn, Minute),
    number_string(S, Second),
    
    utils:stringBuilder([Day, "/", Month, "/", Year, " ", Hour, ":", Minute, ":", Second], "", Text).

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

showReservationList([], Aux, Aux).
showReservationList([H|T], Aux, Text) :-
    showReservation(H, Str),
    string_concat(Str, "\n-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n\n", Part),
    string_concat(Aux, Part, Intermediate),
    showReservationList(T, Intermediate, Text).

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

showDay(Date, Text) :-
    Date = date(Y,M,D),
    number_string(Y, Year),
    number_string(M, Month),
    number_string(D, Day),
    
    utils:stringBuilder([Day, "/", Month, "/", Year], "", Text).

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

createReports(_, [], Aux, Aux).
createReports(Day, [H|T], Aux, Text):-
    createReportForTheRoom(Day, H, Line),
    string_concat(Aux, Line, Intermediate),

    createReports(Day, T, Intermediate, Text).

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