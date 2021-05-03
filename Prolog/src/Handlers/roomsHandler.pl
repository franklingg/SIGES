:- module(roomsHandler, []).

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

cleanReservations(TimeNow, CodeStr) :-
    getRoom(CodeStr, R),
    R = dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization),
    exclude(hasPassed(TimeNow), Schedule, NewSchedule),
    NewRoom = dataHandler:room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(CodeStr),
    dataHandler:saveRoom(NewRoom).

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

