:- module(dataHandler, [saveUser/1, existsUserFile/0, cleanUsers/0, readUsers/0,
                        notExistingUser/1, writeUsers/0, saveRoom/1, existsRoomFile/0, 
                        cleanRooms/0, readRooms/0, notExistingRoom/1, writeRooms/0, 
                        category/1, category/2, resourceKind/1, resourceKind/2]).

% DATATYPES
% :- dynamic userFull/5.
% :- dynamic user/3.

%:- dynamic room/6. % (code, schedule, resources, category, capacity, localization)
%:- dynamic res/2. % (name_of_resource, quantity)
%:- dynamic reservation/4. % (requester, description, start_time, finish_time)

category('Laboratory').
category('Auditorium').
category('Classroom').
category('Office').
category('Warehouse').

category('Laboratory', "Laboratório").
category('Auditorium', "Auditório").
category('Classroom', "Sala de aula").
category('Office', "Escritório").
category('Warehouse', "Depósito").

resourceKind('Projector').
resourceKind('Microscope').
resourceKind('Desk').
resourceKind('Computer').
resourceKind('Board').
resourceKind('AirConditioner').
resourceKind('Projector', "Projetor").
resourceKind('Microscope', "Microscópio").
resourceKind('Desk', "Birô").
resourceKind('Computer', "Computador").
resourceKind('Board', "Quadro").
resourceKind('AirConditioner', "Ar Condicionado").


% HANDLER
cleanUsers:- retractall(userFull/5).

existsUserFile :- exists_file('data/users.bd').

writeUsers:- tell('data/users.bd'),
             listing(userFull/5),
             told,
             cleanUsers.

readUsers:- consult('data/users.bd').

notExistingUser(Email):-
    readUsers,
    findall(_,userFull(_,Email,_,_,_),List),
    cleanUsers, 
    List=[],!;
    fail.

saveUser(U):- assertz(U), writeUsers.

%ROOMS

cleanRooms:- retractall(room/6).

existsRoomFile:- exists_file('data/rooms.bd').

writeRooms:- tell('data/rooms.bd'),
             listing(room),
             told,
             cleanRooms.

readRooms:- consult('data/rooms.bd').

notExistingRoom(Code):-
    readRooms,
    findall(_, dataHandler:room(Code, _, _, _, _, _), List),
    cleanRooms,
    List = [], !;
    fail.

saveRoom(R):- assertz(R), writeRooms.