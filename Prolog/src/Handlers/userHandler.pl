:- module(userHandler, [createUser/4, checkValidEmail/1, checkValidPassword/1, 
                        checkValidName/1, login/2]).

:- use_module(library(crypto)).

:- use_module('./../utils.pl').
:- use_module('./dataHandler.pl').

createUser(Name, Email, Password, AdmChar):-
    utils:timeNow(D),string_lower(AdmChar, L),atom_string(R,L),
    (R='s'->IsAdm=true;R='n'->IsAdm=false),
    (\+ dataHandler:existsUserFile,!;dataHandler:notExistingUser(Email)),
    crypto_password_hash(Password, PasswordHash),
    U=dataHandler:userFull(Name, Email, PasswordHash, IsAdm, D),
    dataHandler:saveUser(U),
    login(Email, Password);
    errorHandler:promptError(7),fail.

checkValidEmail(Email):-
    wildcard_match('[a-z]*@[a-z]*.[a-z]*', Email, [case_sensitive(false)]),!;
    errorHandler:promptError(2),fail.

checkValidPassword(Password):-
    string_length(Password, Length), Length >= 8, !;
    errorHandler:promptError(3),fail.

checkValidName(Name):-
    string_length(Name, Length), Length >= 2, !;
    errorHandler:promptError(6),fail.
    
login(Email,Password):-
    dataHandler:readUsers,
    bagof(_,dataHandler:userFull(Name,Email,PasswordHash,IsAdm,_),List),
    dataHandler:cleanUsers,
    List\=[],crypto_password_hash(Password,PasswordHash),!, 
    assertz(dataHandler:user(Name,Email,IsAdm));
    errorHandler:promptError(4),fail.

deleteUser(Email):-
    dataHandler:readUsers, 
    findall(_,dataHandler:userFull(_,Email,_,_,_),List),List\=[],
    retract(dataHandler:userFull(_,Email,_,_,_)),
    dataHandler:writeUsers,!;
    errorHandler:promptError(8),fail.