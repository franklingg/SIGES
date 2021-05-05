:- module(roomsHandler, [createRoom/5, getRoom/2, deleteRoom/1, makeReservation/5,
                         deleteReservation/3, findReservation/4, editReservation/5,
                         searchRoomsCapacity/2, searchRoomsCategory/2, searchRoomsRequester/2,
                         searchRoomsResources/2, searchRoomsTime/3, cleanAllReservations/0,
                         printCategories/0, printResources/0, showRoom/2, createReportForTheRoom/3,
                         createReportForTheDay/2]).

:- use_module('./../utils.pl').
:- use_module('./dataHandler.pl').

createRoom(CodeStr, Resources, Category, Capacity, Localization) :-
    string_upper(CodeStr, Code),
    R = dataHandler:room(Code, [], Resources, Category, Capacity, Localization),
    dataHandler:saveRoom(R).

getRoom(CodeStr, R) :-
    string_upper(CodeStr, Code),
    dataHandler:readRooms,
    findall(_, dataHandler:room(Code, _, _, _, _, _), List),
    dataHandler:cleanRooms,
    List\=[],
    List = [R|_].

deleteRoom(CodeStr):-
    string_upper(CodeStr, Code),
    dataHandler:readRooms, 
    findall(_,dataHandler:room(Code, _, _, _, _, _),List),List\=[],
    retract(dataHandler:room(Code, _, _, _, _, _)),
    dataHandler:writeRooms.

isFree([], _, _).

isFree([H|T], StartTime, FinishTime) :-
    H(_, _, StartRoom, FinishRoom),
    (StartTime @=< StartRoom, FinishTime @=< FinishRoom,
    !;
    StartTime @>= StartRoom, FinishTime @>= FinishRoom),
    isFree(T, StartTime, FinishTime).

makeReservation(CodeStr, UserName, Description, StartTime, FinishTime) :-
    getRoom(CodeStr, R),
    R = dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization),
    isFree(Schedule, StartTime, FinishTime),
    NewReservation = dataHandler:reservation(UserName, Description, StartTime, FinishTime),
    append([Schedule, [NewReservation]], NewSchedule),
    NewRoom = dataHandler:room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(CodeStr),
    dataHandler:saveRoom(NewRoom).

existsReservation([], _, _) :-
    false.

existsReservation([H|T], UserName, StartTime) :-
    H = dataHandler:reservation(UserName, _, StartTime, _),
    !;
    existsReservation(T, UserName, StartTime).

matchTime(StartTime, Res) :-
    Res = dataHandler:reservation(_, _, StartTime, _).

deleteReservation(CodeStr, UserName, StartTime) :-
    getRoom(CodeStr, R),
    R = dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization),
    existsReservation(Schedule, UserName, StartTime, Res),
    
    exclude(matchTime(StartTime), Schedule, NewSchedule),
    NewRoom = dataHandler:room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(CodeStr),
    dataHandler:saveRoom(NewRoom).
    
findReservation(CodeStr, StartTime, UserName, Res) :-
    getRoom(CodeStr, R),
    R = dataHandler:room(_, Schedule, _, _, _, _),
    include(matchTime(StartTime), Schedule, Matching), Matching\=[],
    Matching = [Res|_].

editReservation(CodeStr, UserName, CurrentStartTime, NewStartTime, NewFinishTime) :-
    getRoom(CodeStr, R),
    R = dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization),
    existsReservation(Schedule, UserName, CurrentStartTime),
    isFree(Schedule, NewStartTime, NewFinishTime),

    findReservation(CodeStr, CurrentStartTime, UserName, Res),
    Res = dataHandler:reservation(Requester, Description, _, _),
    NewReservation = dataHandler:reservation(Requester, Description, NewStartTime, NewFinishTime),
    exclude(matchTime(StartTime), Schedule, DeletedSchedule),
    append([DeletedSchedule, [NewReservation]], NewSchedule),
    NewRoom = dataHandler:room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(CodeStr),
    dataHandler:saveRoom(NewRoom).

hasPassed(TimeNow, Res) :-
    Res = dataHandler:reservation(_, _, _, FinishTime),
    TimeNow @>= FinishTime.

cleanReservations(TimeNow, R) :-
    R = dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization),
    exclude(hasPassed(TimeNow), Schedule, NewSchedule),
    NewRoom = dataHandler:room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(CodeStr),
    dataHandler:saveRoom(NewRoom).

cleanAllReservationsAux([], _).

cleanAllReservationsAux([H|T], TimeNow) :-
    cleanReservations(TimeNow, H),
    cleanAllReservationsAux(T, TimeNow).


cleanAllReservations :-
    dataHandler:readRooms,
    findall(_, dataHandler:room(_, _, _, _, _, _), List),
    dataHandler:cleanRooms,
    List\=[],
    timeNow(TimeNow),

    cleanAllReservationsAux(List, TimeNow).

hasCategory(Category, Room) :-
    Room = dataHandler:room(_, _, _, Category, _, _).

searchRoomsCategory(Category, Rooms) :-
    dataHandler:readRooms,
    findall(_, dataHandler:room(_, _, _, _, _, _), List),
    dataHandler:cleanRooms,
    List\=[],
    include(hasCategory(Category), List, Rooms).

hasCapacity(Capacity, Room) :-
    Room = dataHandler:room(_, _, _, _, RoomCap, _),
    RoomCap >= Capacity.

searchRoomsCapacity(Capacity, Rooms) :-
    dataHandler:readRooms,
    findall(_, dataHandler:room(_, _, _, _, _, _), List),
    dataHandler:cleanRooms,
    List\=[],
    include(hasCapacity(Capacity), List, Rooms).

hasTime(StartTime, FinishTime, Room) :-
    Room = dataHandler:room(_, Schedule, _, _, _, _),
    isFree(Schedule, StartTime, FinishTime).

searchRoomsTime(StartTime, FinishTime, Rooms) :-
    dataHandler:readRooms,
    findall(_, dataHandler:room(_, _, _, _, _, _), List),
    dataHandler:cleanRooms,
    List\=[],
    include(hasCapacity(Capacity), List, Rooms).

resourceIsEnough([], _) :-
    false.

resourceIsEnough([H|T], Resource) :-
    H = dataHandler:resource(Name, Amount),
    Resource = dataHandler:resource(RequiredName, RequiredAmount),
    Name = RequiredName,
    Amount >= RequiredAmount,
    !;
    resourceIsEnough(T, Resource).

containsAllResources(RequiredResources, Room) :-
    Room = dataHandler:room(_, _, Resources, _, _, _),
    exclude(resourceIsEnough(Resources), RequiredResources, Result),
    Result = [].

searchRoomsResources(Resources, Rooms) :-
    dataHandler:readRooms,
    findall(_, dataHandler:room(_, _, _, _, _, _), List),
    dataHandler:cleanRooms,
    List\=[],
    include(containsAllResources(Resources), List, Rooms).

hasRequester(UserName, Reservation) :-
    Reservation = dataHandler:reservation(UserName, _, _, _).

wasReservedBy(UserName, Room) :-
    Room = dataHandler:room(_, Schedule, _, _, _, _),
    include(hasRequester(UserName), Schedule, List),
    List \= [].

searchRoomsRequester(UserName, Rooms) :-
    dataHandler:readRooms,
    findall(_, dataHandler:room(_, _, _, _, _, _), List),
    dataHandler:cleanRooms,
    List\=[],
    include(wasReservedBy(UsewrName), List, Rooms).
    
printCategories :-
    writeln('Qual categoria você deseja escolher?\n\
            \[L]aboratório\n\
            \[A]uditório\n\
            \[S]ala de aula\n\
            \[E]scritório\n\
            \[D]epósito')

printResources :-
    writeln('Qual recurso você deseja escolher?\n\
            \[P]rojetor\n\
            \[M]icroscópio\n\
            \[B]irô\n\
            \[C]omputador\n\
            \[Q]uadro\n\
            \[A]r condicionado')

showResourcesList([], Aux, Text) :-
    Text = Aux.

showResourcesList([H|T], Aux, Text) :-
    H = dataHandler:resource(Name, Amount),
    number_string(Amount, StrAmount),

    string_concat(Name, ': ', Begin),
    string_concat(Begin, StrAmount, Full),
    string_concat(Full, '\n', Line),

    string_concat(Aux, Line, Intermediate),
    showResourcesList(T, Intermediate, Text).
    

showRoom(Room, Text) :-
    Room = dataHandler:room(Code, _, Resources, Category, Capacity, Localization),
    Category = category(RoomCat),
    number_string(Capacity, Cap),
    showResourcesList(Resources, '', ResList),
    
    L1 = 'Dados da sala: ',
    string_concat('Categoria: ', RoomCat, L2),
    string_concat('Capacidade: ', Cap, L3),
    string_concat('Localização: ', Localization, L4),
    string_concat('Recursos: \n', ResList, L5),

    utils:stringBuilder([L1, '\n', L2, '\n', L3, '\n', L4, '\n', L5], '', Text).

showDate(Date, Text) :-
    Date = date(Y,M,D,H,Mn,S,_,_,_),
    number_string(Y, Year),
    number_string(M, Month),
    number_string(D, Day),
    number_string(H, Hour),
    number_string(Mn, Minute),
    number_string(S, Second),
    
    utils:stringBuilder([Day, '/', Month, '/', Year, ' ', Hour, ':', Minute, ':', Second], '', Text).

showReservation(Reservation, Text) :-
    Reservation = dataHandler:reservation(Requester, Description, StartTime, FinishTime),
    showDate(StartTime, Start),
    showDate(FinishTime, Finish),  

    L1 = 'Dados da reserva: ',
    string_concat('Início: ', Start, L2),
    string_concat('Fim: ', Finish, L3),
    string_concat('Responsável: ', Requester, L4),
    string_concat('Motivo: ', Description, L5),

    utils:stringBuilder([L1, '\n', L2, '\n', L3, '\n', L4, '\n', L5], '', Text).

showReservationList([], Aux, Text) :-
    Text = Aux.

showReservationList([H|T], Aux, Text) :-
    showReservation(H, Str),
    string_concat(Str, '\n-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n\n', Intermediate),

    showReservationList(T, Intermediate, Text).

filterReservations([], _, Aux, List) :-
    List = Aux.

filterReservations([H|T], Day, Aux, List) :- %Day é um date/3, diferente do Start e FinishTime, que são date/9
    H = dataHandler:reservation(Requester, Description, StartTime, FinishTime),
    StartTime = date(Y,M,D,_,_,_,_,_,_),
    ResDay = date(Y, M, D),

    (ResDay = Day,
    append([Aux], [H], Intermediate),
    !;
    Intermediate = Aux,),

    filterReservations(T, Day, Intermediate, List).

showDay(Day, Text) :-
    Day = date(Y,M,D),
    number_string(Y, Year),
    number_string(M, Month),
    number_string(D, Day),

    stringBuilder([Day, '/', Month, '/', Year], '', Text)

createReportForTheRoom(Day, Room, Text) :-
    R = dataHandler:room(Code, Schedule, _, _, _, _),
    Day = date(Y, M, D),
    showDay(Day, D),

    filterReservations(Schedule, Day, [], Reservations),
    string_concat('Relatório de ocupação para a sala ', Code, Str1),
    string_concat(Str1, ' no dia: ', Str2),
    string_concat(Str2, D, Str3),
    string_concat(Str3, ':\n\n', L1),

    showReservationList(Reservations, '', L2),

    string_concat(L1, L2, Text).

createReports([], Aux, Text) :-
    Text = Aux.

createReports([H|T], Aux, Text):-
    createReportForTheRoom(H, '', Line),
    string_concat(Aux, Line, Intermediate),

    createReports(T, Intermediate, Text).

createReportForTheDay(Day, Text) :-
    dataHandler:readRooms,
    findall(_, dataHandler:room(_, _, _, _, _, _), List),
    dataHandler:cleanRooms,
    List\=[],

    createReports(List, '', Text).
