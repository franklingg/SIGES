/** <module> UserHandler
* Description : Módulo contendo as operações básicas de manipulação de usuários no sistema SIGES.
*/
:- module(userHandler, [createUser/4, checkValidEmail/1, checkValidPassword/1, 
                        checkValidName/1, login/2]).

:- encoding(utf8).

:- use_module(library(crypto)).

:- use_module('./../utils.pl').
:- use_module('./dataHandler.pl').

/* 
* Dadas três Strings contendo respectivamente o nome, e-mail, a senha, e um valor booleano indicando se trata-se de um administrador, esta função cria um novo usuário e o armazena no sistema.
*/
createUser(Name, Email, Password, IsAdm):-
    utils:timeNow(D),
    (\+ dataHandler:existsUserFile,!;dataHandler:notExistingUser(Email)),
    crypto_password_hash(Password, PasswordHash),
    U= userFull(Name, Email, PasswordHash, IsAdm, D),
    dataHandler:saveUser(U),
    login(Email, Password),!;
    errorHandler:promptError(7),fail.

/*
* Função que checará se o email passado pelo usuário é um email válido. 
*/
checkValidEmail(Email):-
    wildcard_match("[a-z]*@[a-z]*.[a-z]*", Email, [case_sensitive(false)]),!;
    errorHandler:promptError(2),fail.

/*
* Função que checará se a senha passado pelo usuário é uma senha válida. 
*/
checkValidPassword(Password):-
    string_length(Password, Length), Length >= 8, !;
    errorHandler:promptError(3),fail.

/*
* Função que checará se o nome passado pelo usuário é um nome válido. 
*/
checkValidName(Name):-
    string_length(Name, Length), Length >= 2, !;
    errorHandler:promptError(6),fail.

/* 
* Dado um email e uma senha, esta função logará um usuário do sistema.
*/
login(Email,Password):-
    dataHandler:readUsers,
    bagof(_,dataHandler:userFull(Name,Email,Hash,IsAdm,_),List),
    dataHandler:cleanUsers,
    atom_string(PasswordHash, Hash),
    List\=[],crypto_password_hash(Password,PasswordHash), 
    assertz(dataHandler:user(Name,Email,IsAdm)),!;
    errorHandler:promptError(4),fail.

/*
* Esta função recebe um usuário e o remove do sistema.
*/
deleteUser(Email):-
    dataHandler:readUsers, 
    findall(_,dataHandler:userFull(_,Email,_,_,_),List),List\=[],
    retract(dataHandler:userFull(_,Email,_,_,_)),
    dataHandler:writeUsers,!;
    errorHandler:promptError(8),fail.