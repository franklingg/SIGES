:- module(dataHandler, [saveUser/1, existsUserFile/0, cleanUsers/0, readUsers/0,
                        notExistingUser/1, writeUsers/0, saveRoom/1, existsRoomFile/0, 
                        cleanRooms/0, readRooms/0, notExistingRoom/1, writeRooms/0]).

% DATATYPES
:- dynamic userFull/5.
:- dynamic user/3.
:- dynamic room/6. % (code, schedule, resources, category, capacity, localization)
:- dynamic resource/2. % (name_of_resource, quantity)
:- dynamic category/1. % (name_of_category)
:- dynamic reservation/4. % (requester, description, start_time, finish_time)

% HANDLER
cleanUsers :- retractall(userFull).

existsUserFile :- exists_file('data/users.bd').

writeUsers:- tell('data/users.bd'),
             listing(userFull),
             told,
             cleanUsers.

readUsers :- consult('data/users.bd').

notExistingUser(Email):-
    readUsers,
    findall(_,userFull(_,Email,_,_,_),List),
    cleanUsers, 
    List=[],!;
    fail.

saveUser(U):- assertz(U), writeUsers.

%ROOMS

cleanRooms :- retractall(room).

existsRoomsFile :- exists_file('data/rooms.bd').

writeRooms :- tell('data/rooms.bd'),
             listing(room),
             told,
             cleanRooms.

readRooms :- consult('data/rooms.bd').

notExistingRoom(code) :-
    readRooms,
    findall(_, room(Code, _, _, _, _, _), List),
    cleanRooms,
    List = [], !;
    fail.

saveRoom(R) :- assertz(R), writeRooms.