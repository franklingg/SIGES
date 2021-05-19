/** <module> DataHandler
* Description : Módulo contendo as operações de manipulação de dados e persistência do sistema SIGES.
*/
:- module(dataHandler, [saveUser/1, existsUserFile/0, cleanUsers/0, readUsers/0,
                        notExistingUser/1, writeUsers/0, saveRoom/1, existsRoomFile/0, 
                        cleanRooms/0, readRooms/0, notExistingRoom/1, writeRooms/0, 
                        category/1, category/2, checkCategory/1, getCategory/2, 
                        resourceKind/1, resourceKind/2, checkResource/1, getResource/2]).
:- encoding(utf8).

% DATATYPES
% :- dynamic userFull/5.
% :- dynamic user/3.

%:- dynamic room/6. % (code, schedule, resources, category, capacity, localization)
%:- dynamic res/2. % (name_of_resource, quantity)
%:- dynamic reservation/4. % (requester, description, start_time, finish_time)

/* 
* Difinição das Regras de categoria das salas.
*/
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

/*
* Esta função considera uma String, e checa se a categoria da sala esta cadastrada no sistema, caso exista.
*/
checkCategory(CategoryStr):-
    string_length(CategoryStr, L), L = 1,
    string_upper(CategoryStr, Category),
    member(Category, ["L","A","S","E","D"]),!;
    errorHandler:promptError(14),fail.

/*
* Esta função retorna a Categoria da sala cadastrada no sistema, caso exista.
*/
getCategory(CategoryInit, Category):-
    string_upper(CategoryInit, Init),
    (Init = "L"->Category=dataHandler:category('Laboratory'),!;
     Init = "A"->Category=dataHandler:category('Auditorium'),!;
     Init = "S"->Category=dataHandler:category('Classroom'),!;
     Init = "E"->Category=dataHandler:category('Office'),!;
     Init = "D"->Category=dataHandler:category('Warehouse')).

/* 
* Difinição das Regras dos Tipos de recursos disponiveis nas salas.
*/
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

/*
* Esta função considera uma String, e checa os tipos de recursos disponiveis nas salas, caso exista.
*/
checkResource(ResourceStr):-
    string_length(ResourceStr, L), L = 1,
    string_upper(ResourceStr, Resource),
    member(Resource, ["P","M","B","C","Q","A"]),!;
    errorHandler:promptError(14),fail.

/*
* Esta função retorna o tipo recurso da sala cadastrada no sistema, caso exista.
*/
getResource(ResourceInit, Resource):-
    string_upper(ResourceInit, Init),
    (Init = "P"->Resource=dataHandler:resourceKind('Projector'),!;
     Init = "M"->Resource=dataHandler:resourceKind('Microscope'),!;
     Init = "B"->Resource=dataHandler:resourceKind('Desk'),!;
     Init = "C"->Resource=dataHandler:resourceKind('Computer'),!;
     Init = "Q"->Resource=dataHandler:resourceKind('Board'),!;
     Init = "A"->Resource=dataHandler:resourceKind('AirConditioner')).


%HANDLER
cleanUsers:- retractall(userFull/5).

/*
* Esta função verifica se existe o arquino dos usuários cadastrados no sistema.
*/
existsUserFile :- exists_file('data/users.bd').

/*
* Esta função escreve no arquivo dos usuários cadastrados no sistema.
*/
writeUsers:- tell('data/users.bd'),
             listing(userFull/5),
             told,
             cleanUsers.

/*
* Esta função ler o arquivo dos usuários cadastrados no sistema.
*/
readUsers:- consult('data/users.bd').

/*
* Esta função informa que o usuario não esta cadastrado no sistema.
*/
notExistingUser(Email):-
    readUsers,
    findall(_,userFull(_,Email,_,_,_),List),
    cleanUsers, 
    List=[],!;
    fail.

/*
* Esta função salva um usuario no sistema, caso ele ainda não tenha sido cadastrado.
*/
saveUser(U):- assertz(U), writeUsers.

%ROOMS

cleanRooms:- retractall(room/6).

/*
* Esta função verifica se existe o arquino das salas cadastradas no sistema.
*/
existsRoomFile:- exists_file('data/rooms.bd').

/*
* Esta função escreve no arquivo das salas cadastradas no sistema.
*/
writeRooms:- tell('data/rooms.bd'),
             listing(room),
             told,
             cleanRooms.

/*
* Esta função ler o arquivo das salas cadastradas no sistema.
*/
readRooms:- consult('data/rooms.bd').

/*
* Esta função informa que a sala não esta cadastrada no sistema.
*/
notExistingRoom(Code):-
    readRooms,
    findall(_, dataHandler:room(Code, _, _, _, _, _), List),
    cleanRooms,
    List = [], !;
    fail.

/*
* Esta função salva uma sala no sistema, caso ela ainda não tenha sido cadastrada.
*/
saveRoom(R):- assertz(R), writeRooms.