:- module(dataHandler, [saveUser/1, existsUserFile/0, cleanUsers/0, readUsers/0,
                        notExistingUser/1, writeUsers/0]).

% DATATYPES
:- dynamic userFull/5.
:- dynamic user/3.

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