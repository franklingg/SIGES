/** <module> DataHandler
* Módulo contendo as operações de criação, manipulação e persistência de dados no sistema SIGES.
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

/**
 * Definição das categorias das salas.
 */
category('Laboratory').
category('Auditorium').
category('Classroom').
category('Office').
category('Warehouse').
/**
 * Definição das categorias das salas e sua respectiva tradução.
 */
category('Laboratory', "Laboratório").
category('Auditorium', "Auditório").
category('Classroom', "Sala de aula").
category('Office', "Escritório").
category('Warehouse', "Depósito").

/**
 * checkCategory(+CategoryStr:string) is semidet.
 * 
 * Esta função considera uma String, e checa se a categoria da sala é uma das categorias possíveis,
 * imprimido uma mensagem de erro caso contrário.
 * @param CategoryStr String a ser verificada como categoria
 */
checkCategory(CategoryStr):-
    string_length(CategoryStr, L), L = 1,
    string_upper(CategoryStr, Category),
    member(Category, ["L","A","S","E","D"]),!;
    errorHandler:promptError(14),fail.

/**
 * getCategory(+CategoryInit:string, -Category: compound) is semidet.
 *
 * Esta função retorna a categoria da sala cadastrada no sistema, caso exista.
 * @param CategoryInit String com a inicial da categoria a ser buscada
 * @param Category Categoria encontrada a ser retornada
 */
getCategory(CategoryInit, Category):-
    string_upper(CategoryInit, Init),
    (Init = "L"->Category=dataHandler:category('Laboratory'),!;
     Init = "A"->Category=dataHandler:category('Auditorium'),!;
     Init = "S"->Category=dataHandler:category('Classroom'),!;
     Init = "E"->Category=dataHandler:category('Office'),!;
     Init = "D"->Category=dataHandler:category('Warehouse')).

/**
 * Definição dos tipos de recursos disponiveis nas salas.
 */
resourceKind('Projector').
resourceKind('Microscope').
resourceKind('Desk').
resourceKind('Computer').
resourceKind('Board').
resourceKind('AirConditioner').
/**
 * Definição dos tipos de recursos disponiveis nas salas e suas respectivas traduções.
 */
resourceKind('Projector', "Projetor").
resourceKind('Microscope', "Microscópio").
resourceKind('Desk', "Birô").
resourceKind('Computer', "Computador").
resourceKind('Board', "Quadro").
resourceKind('AirConditioner', "Ar Condicionado").

/**
 * checkCategory(+ResourceStr:string) is semidet.
 * 
 * Esta função considera uma String, e checa se o recurso da sala é um dos recursos possíveis,
 * imprimido uma mensagem de erro caso contrário.
 * @param ResourceStr String a ser verificada como recurso
 */
checkResource(ResourceStr):-
    string_length(ResourceStr, L), L = 1,
    string_upper(ResourceStr, Resource),
    member(Resource, ["P","M","B","C","Q","A"]),!;
    errorHandler:promptError(14),fail.

/**
 * getCategory(+ResourceInit:string, -Resource: compound) is semidet.
 *
 * Esta função retorna o recurso da sala cadastrada no sistema, caso exista.
 * @param ResourceInit String com a inicial do recurso a ser buscada
 * @param Resource Recurso encontrado a ser retornado
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
/**
 * cleanUsers is det.
 * 
 * Limpa todos os usuários armazenados na memória.
 */
cleanUsers:- retractall(userFull/5).

/**
 * existsUserFile is det.
 * 
 * Verifica se existe o arquivo de persistência de usuários.
 */
existsUserFile :- exists_file('data/users.bd').

/**
 * writeUsers is det.
 * 
 * Exreve na persistência de usuários todos os usuários registrados na memória.
 */
writeUsers:- tell('data/users.bd'),
             listing(userFull/5),
             told,
             cleanUsers.

/**
 * readUsers is det.
 * 
 * Consulta a persistência e armazena na memória todos os usuários registrados.
 */
readUsers:- consult('data/users.bd').

/**
 * notExistingUser(+Email:string) is semidet.
 * 
 * Verifica se o usuário com o email fornecido é um usuário cadastrado no sistema,
 * falhando caso contrário.
 * @param Email Email do usuário a ser buscado
 */
notExistingUser(Email):-
    readUsers,
    findall(_,userFull(_,Email,_,_,_),List),
    cleanUsers, 
    List=[],!;
    fail.

/**
 * saveUser(+U:compound) is det.
 * 
 * Salva o usuário fornecido na persistência.
 * @param U Usuário a ser armazenado
 */
saveUser(U):- assertz(U), writeUsers.

%ROOMS
/**
 * cleanRooms is det.
 * 
 * Limpa todas as salas armazenados na memória.
 */
cleanRooms:- retractall(room/6).

/**
 * existsRoomFile is det.
 * 
 * Verifica se existe o arquivo de persistência de salas.
 */
existsRoomFile:- exists_file('data/rooms.bd').

/**
 * writeRooms is det.
 * 
 * Exreve na persistência de salas todas as salas registrados na memória.
 */
writeRooms:- tell('data/rooms.bd'),
             listing(room),
             told,
             cleanRooms.

/**
 * readRooms is det.
 * 
 * Consulta a persistência e armazena na memória todas as salas registrados.
 */
readRooms:- consult('data/rooms.bd').

/**
 * notExistingRoom(+Code:string) is semidet.
 * 
 * Verifica se a sala com o código fornecido é uma sala cadastrada no sistema,
 * falhando caso contrário.
 * @param Code Código da sala a ser buscada
 */
notExistingRoom(Code):-
    readRooms,
    findall(_, dataHandler:room(Code, _, _, _, _, _), List),
    cleanRooms,
    List = [], !;
    fail.

/**
 * saveRoom(+R:compound) is det.
 * 
 * Salva a sala fornecida na persistência.
 * @param R Sala a ser armazenado
 */
saveRoom(R):- assertz(R), writeRooms.