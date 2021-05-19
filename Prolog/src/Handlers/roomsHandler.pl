/** <module> RoomsHandler
* Description : Módulo contendo as operações básicas para manipulação de salas no sistema SIGES.
*/
:- module(roomsHandler, [createRoom/5, getRoom/2, deleteRoom/1, makeReservation/5,
                         deleteReservation/3, findReservation/3, editReservation/5,
                         searchRoomsCapacity/2, searchRoomsCategory/2, searchRoomsRequester/2,
                         searchRoomsResources/2, searchRoomsTime/3, cleanAllReservations/0,
                         printCategories/0, printResources/0, showRoom/2, createReportForTheRoom/3,
                         createReportForTheDay/2,checkNewRoomCode/1]).
:- encoding(utf8).
:- use_module("./../utils.pl").
:- use_module("./dataHandler.pl").

/**
 * createRoom(+CodeStr:string, +Resources:compound, +Category:compound, +Capacity:int, +Localization:string) is semidet.
 * 
 * Recebe todas as informações pertinentes a uma sala e a cria com nenhuma reserva. Sala se a sala já estiver cadastrada no sistema.
 * @param CodeStr O código da sala, não é case-sensitive.
 * @param Resources Uma lista contendo todos os recursos oferecidos pela sala.
 * @param Category A categoria da sala.
 * @param Capacity A quantidade de pessoas comportadas pela sala.
 * @param Localization Breve descrição da localização da sala.
 */
createRoom(CodeStr, Resources, Category, Capacity, Localization) :-
    string_upper(CodeStr, Code),
    dataHandler:notExistingRoom(Code),
    R = room(Code, [], Resources, Category, Capacity, Localization),
    dataHandler:saveRoom(R), !;
    errorHandler:promptError(9), fail.

/**
 * getRoom(+CodeStr:string, -R:compound) is semidet.
 * 
 * Recebe o código de uma sala (não é case-sensitive) e retorna a sala correspondente cadastrada no sistema. Falha se a sala não existir.
 * @param CodeStr O código da sala que se deseja buscar.
 * @param R A sala consultada.
 */
getRoom(CodeStr, R) :-
    string_upper(CodeStr, Code),
    dataHandler:readRooms,
    bagof(_, dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization), List),
    dataHandler:cleanRooms,
    List\=[],
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),!;
    errorHandler:promptError(10), fail.

/**
 * deleteRoom(+CodeStr:string) is semidet.
 * 
 * Recebe um código (não case-sensitive) de uma sala e a remove do sistema. Falha se a sala não existir.
 * @param CodeStr O código da sala que se deseja remover.
 */
deleteRoom(CodeStr):-
    string_upper(CodeStr, Code),
    dataHandler:readRooms, 
    findall(_,dataHandler:room(Code, _, _, _, _, _),List),List\=[],
    retract(dataHandler:room(Code, _, _, _, _, _)),
    dataHandler:writeRooms,!;
    errorHandler:promptError(10), fail.

/**
 * isFree(+Schedule:list, +StartTime:compound, +FinishTime:compound) is semidet.
 * 
 * Recebe a lista de reservas de uma sala e um horário de início e fim. Sucede caso nenhuma das reservas na lista chocarem com o intervalo de horário fornecido.
 * @param Schedule A lista de reservas da sala.
 * @param StartTime O date/9 representando o horário em que o intervalo consultado inicia.
 * @param FinishTime O date/9 representando o horário em que o intervalo consultado acaba.
 */
isFree([], _, _).
isFree([H|T], StartTime, FinishTime):-
    H = reservation(_, _, StartTimeRoom, FinishTimeRoom),
    utils:xor((StartTimeRoom @>= FinishTime), (FinishTimeRoom @=< StartTime )),
    isFree(T, StartTime, FinishTime).

/**
 * makeReservation(+CodeStr:string, +UserName:string, +Description:string, +StartTime:compound, +FinishTime:compound) is semidet.
 * 
 * Recebe o código (não case-sensitive) de uma sala e cria nela uma reserva com as outras informações fornecidas. Falha se a sala estiver ocupada no horário especificado.
 * @param CodeStr O código da sala que se deseja reservar.
 * @param UserName O nome do usuário responsável pela reserva.
 * @param Description Breve descrição do evento ocorrendo na sala durante o horário reservado.
 * @param StartTime O date/9 representando o horário de início da reserva.
 * @param FinishTime O date/9 representando o horário de final da reserva.
 */
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

/**
 * existsReservation(+Schedule:list, +UserName:string, +StartTime:compound) is semidet.
 * 
 * Recebe uma lista de reservas, um nome de usuário e um horário de início. Sucede se existe na lista uma reserva com estas informações.
 * @param Schedule A lista de reservas de uma sala.
 * @param Username O nome do usuário responsável pela reserva.
 * @param StartTime O horário em que a reserva se inicia.
 */
existsReservation([], _, _) :- false.
existsReservation([H|T], UserName, StartTime) :-
    H = reservation(UserName, _, StartTime, _),!;
    existsReservation(T, UserName, StartTime).

/**
 * matchTime(+StartTime:compound, +Res:compound) is semidet.
 * 
 * Regra auxiliar. Recebe um horário e uma reserva e sucede se a reserva inicia no horário passado.
 * @param StartTime O horário comparado.
 * @param Res A reserva comparada.
 */
matchTime(StartTime, Res) :-
    Res = reservation(_, _, StartTime, _).

/**
 * deleteReservation(+CodeStr:string, +UserName:string, +StartTime:compound) is semidet.
 * 
 * Recebe as informações pertinentes e remove uma reserva do sistema. Falha se a reserva não existir.
 * @param CodeStr O código (não case-sensitive) da sala onde está a reserva.
 * @param UserName O nome do usuário responsável pela reserva.
 * @param StartTime O date/9 representando o horário em que a reserva se inicia.
 */
deleteReservation(CodeStr, UserName, StartTime) :-
    getRoom(CodeStr, R),
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),
    existsReservation(Schedule, UserName, StartTime),
    
    exclude(matchTime(StartTime), Schedule, NewSchedule),
    NewRoom = room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(CodeStr),
    dataHandler:saveRoom(NewRoom), !;
    errorHandler:promptError(12), fail.

/**
 * findReservation(+CodeStr:string, +StartTime:compound, -Res:compound) is semidet
 * 
 * Recebe um código (não case-sensitive) de uma sala e um horário e retorna a reserva equivalente. Falha se a reserva não existir.
 * @param O código da sala onde a reserva está.
 * @param StartTime O date/9 representando o horario de início da reserva.
 * @param Res A reserva buscada.
 */
findReservation(CodeStr, StartTime, Res) :-
    getRoom(CodeStr, R),
    R = room(_, Schedule, _, _, _, _),
    include(matchTime(StartTime), Schedule, Matching), Matching\=[],
    Matching = [Res|_].

/**
 * editReservation(+CodeStr:String, +UserName:string, +CurrentStartTime:compound, +NewStartTime:compound, +NewFinishTime:compound) is semidet.
 * 
 * Recebe as informações de uma sala e reserva, e edita esta reserva para alterar o seu horário. Falha se o novo horário estiver ocupado ou se a reserva não existir.
 * @param CodeStr O código (não case-sensitive) da sala onde a reserva foi feita.
 * @param UserName O nome do usuário responsável pela reserva.
 * @param CurrentStartTime O date/9 com o atual horário de início da reserva.
 * @param NewStartTime O date/9 com o novo horário de início da reserva.
 * @param NewFinishTime O date/9 com o novo horário de fim da reserva.
 */
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

/**
 * hasPassed(+TimeNow:compound, +Res:compound) is semidet.
 * 
 * Regra auxiliar. Recebe o horário atual e uma reserva e sucede se a reserva acaba antes do horário atual.
 * @param TimeNow O horário atual em formato date/9.
 * @param Res A reserva verificada.
 */
hasPassed(TimeNow, Res) :-
    Res = reservation(_, _, _, FinishTime),
    TimeNow @>= FinishTime.

/**
 * cleanReservations(+TimeNow:compound, +R:compound) is det.
 * 
 * Recebe O horário atual e uma sala e remove todas as reservas da sala cujo horário final é antes do horário atual.
 * @param TimeNow o horário atual em date/9.
 * @param R a sala.
 */
cleanReservations(TimeNow, R) :-
    R = room(Code, Schedule, Resources, Category, Capacity, Localization),
    exclude(hasPassed(TimeNow), Schedule, NewSchedule),
    NewRoom = room(Code, NewSchedule, Resources, Category, Capacity, Localization),
    deleteRoom(Code),
    dataHandler:saveRoom(NewRoom).

/**
 * cleanAllReservationsAux(+Rooms:list, +TimeNow:compound) is det
 * 
 * Recursão auxiliar que recebe o horário atual e uma lista de salas e remove das salas na lista as reservas que acabam antes do horário passado.
 * @param Rooms A lista de salas que se deve "limpar".
 * @param TimeNow O horário de referência.
 */
cleanAllReservationsAux([], _).
cleanAllReservationsAux([H|T], TimeNow) :-
    cleanReservations(TimeNow, H),
    cleanAllReservationsAux(T, TimeNow).

/**
 * cleanAllReservations() is det.
 * 
 * Remove todas as reservas finalizadas de todas as salas do sistema.
 */
cleanAllReservations :-
    fetchRooms(Rooms),
    timeNow(TimeNow),
    cleanAllReservationsAux(Rooms, TimeNow).

/**
 * filterCategory(+Room:list, +Category:compound, +Aux:list, -List:list) is det.
 * 
 * Filtro auxiliar que filtra uma lista de salas para encontrar nela apenas aquelas que pertencem à categoria especificada.
 * @param Rooms A lista de salas analisadas.
 * @param Category A categoria que se deseja filtrar.
 * @param Aux Lista auxiliar para se fazer a recursão. deve ser iniciada como uma lista vazia, preferencialmente.
 * @param List A lista contendo todas as salas que passaram o filtro, ou seja, as que pertencem à categoria especificada.
 */
filterCategory([], _, Aux, Aux).
filterCategory([H|T], Category, Aux, List):-
    H = room(_, _, _, Category, _, _),
    append(Aux, [H], Intermediate), filterCategory(T, Category, Intermediate, List), !;
    filterCategory(T, Category, Aux, List).

/**
 * searchRoomsCategory(+Category:compound, -Rooms:list) is det.
 * 
 * Recebe uma categoria e retorna todas as salas do sistema que pertencem a esta categoria.
 * @param Category A categoria que se deseja buscar.
 * @param Rooms A lista com as salas do sistema que pertencem à categoria buscada.
 */
searchRoomsCategory(Category, Rooms) :-
    fetchRooms(Salas),
    filterCategory(Salas, Category, [], Rooms).

/**
 * filterCapacity(+Room:list, +Capacity:int, +Aux:list, -List:list) is det.
 * 
 * Filtro auxiliar que filtra uma lista de salas para encontrar nela apenas aquelas cuja capacidade é igual ou maior do que a especificada.
 * @param A lista de salas analisadas.
 * @param Capacity A capacidade que se deseja filtrar.
 * @param Aux Lista auxiliar para se fazer a recursão. deve ser iniciada como uma lista vazia, preferencialmente.
 * @param List A lista contendo todas as salas que passaram no filtro, ou seja, aquelas com a capacidade suficiente.
 */
filterCapacity([], _, Aux, Aux).
filterCapacity([H|T], Capacity, Aux, List):-
    H = room(_, _, _, _, Cap, _),
    Cap >= Capacity,
    append(Aux, [H], Intermediate), filterCapacity(T, Capacity, Intermediate, List), !;
    filterCapacity(T, Capacity, Aux, List).

/**
 * searchRoomsCapacity(+Capacity:int, -Rooms:list) is det.
 * 
 * Recebe um número inteiro e retorna a lista de todas as salas do sistema cuja capacidade é maior ou igual a este número.
 * @param Capacity A capacidade que se deseja filtrar.
 * @param Rooms A lista com as salas do sistema que comportam a quantidade pretendida de ocupantes.
 */
searchRoomsCapacity(Capacity, Rooms) :-
    fetchRooms(Salas),
    filterCapacity(Salas, Capacity, [], Rooms).

/**
 * mountRooms(+Informations:list, +Aux:list, -Rooms:list) is det.
 * 
 * Recebe uma lista de listas (cada uma destas contendo as informações de uma sala) e monta uma lista de salas.
 * @param Informations Uma lista de listas, cada uma destas contendo as informações de uma sala.
 * @param Aux Lista auxiliar usada como ponto de partida para a recursão, deve ser inicializada como uma lsta vazia.
 * @param Rooms Lista contendo todas as salas cujas informações foram passadas.
 */
mountRooms([], Aux, Aux).
mountRooms([H|T], Aux, Rooms) :-
    H = [Code, Schedule, Resources, Category, Capacity, Localization],
    Room = room(Code, Schedule, Resources, Category, Capacity, Localization),
    append(Aux, [Room], Intermediate),
    mountRooms(T, Intermediate, Rooms).    

/**
 * fetchRooms(-Rooms:list) is det.
 * 
 * Retorna uma lista com todas as salas do sistema.
 * @param Rooms A lista com todas as salas do sistema.
 */
fetchRooms(Rooms) :-
    dataHandler:readRooms,
    findall([Code, Schedule, Resources, Category, Capacity, Localization], dataHandler:room(Code, Schedule, Resources, Category, Capacity, Localization), List),
    mountRooms(List, [], Rooms),
    dataHandler:cleanRooms.

/**
 * filterTime(+Rooms:list, +StartTime:compound, +FinishTime:compound, +Aux:list, -List:list) is det.
 * 
 * Filtro auxiliar que, dada uma lista de salas, encontra aquelas que têm um determinado horário livre e as retorna. 
 * @param Rooms A lista de salas analisadas.
 * @param StartTime O date/9 representando o início do horário a ser consultado.
 * @param FinishTime O date/9 representando o fim do horário a ser consultado.
 * @param Aux Lista auxiliar da recursão e usada como ponto inicial. deve ser inicializada como uma lista vazia.
 * @param List Lista de rtorno com todas as salas analisadas que passaram o filtro, ou seja, aquelas que têm o horário consultado livre.
 */
filterTime([], _, _, Aux, Aux).
filterTime([H|T], StartTime, FinishTime, Aux, List):-
    H = room(_, Schedule, _, _, _, _),
    isFree(Schedule, StartTime, FinishTime),
    append(Aux, [H], Intermediate), filterTime(T, StartTime, FinishTime, Intermediate, List), !;
    filterTime(T, StartTime, FinishTime, Aux, List).

/**
 * searchRoomsTime(+StartTime:compound, +FinishTime:compound, -Rooms: list) is det.
 * 
 * Recebe um horário de início e um de fim e retorna a lista de todas as salas que têm este intervalo de horário livre.
 * @param StartTime O date/9 representando o horário de início do intervalo consultado.
 * @param FinishTime O date/9 representando o horário de fim do intervalo consultado.
 * @param Rooms A lista das salas do sistema que estão livres no horário especificado.
 */
searchRoomsTime(StartTime, FinishTime, Rooms):-
    fetchRooms(Salas),
    filterTime(Salas, StartTime, FinishTime, [], Rooms).

/**
 * resourceIsEnough(+Resources:list, +Res:compound) is semidet.
 * 
 * Regra auxiliar. recebe uma lista de recursos oferecidos por uma sala e um recurso, e sucede se este recurso existe na sala na quantidade igual ou maior que a requerida.
 * @param Resources A lista de recursos oferecidos por uma determinada sala.
 * @param um recurso que se procura.
 */
resourceIsEnough([], _) :- fail.
resourceIsEnough([H|T], Res) :-
    H = res(Kind, Amount),
    Res = res(RequiredKind, RequiredAmount),
    Kind = RequiredKind,
    Amount >= RequiredAmount, !;
    resourceIsEnough(T, Res).

/**
 * containsAllResources(+Resources:list, +RequiredResults:list) is semidet.
 * 
 * Recebe uma lista de recursos de uma sala e uma lista de recursos requeridos. Sucede se a sala possuir todos os recursos requeridos e em quantidade igual ou maior que a necessária.
 * @param Resources A lista de recursos da sala.
 * @param RequiredResources A lista com tyodos os recursos que se deseja verificar se a sala oferece.
 */
containsAllResources(_, []).
containsAllResources(Resources, [H|T]) :-
    resourceIsEnough(Resources, H),
    containsAllResources(Resources, T),!;
    fail.

/**
 * filterResources(+Rooms:list, +RequiredResources:list, +Aux:list, -List:list) is det.
 * 
 * Filtro auxiliar que, dada uma lista de salas, encontra aquelas que oferecem os recursos pedidos e as retorna.
 * @param Rooms A lista de salas analisadas.
 * @param RequiredResources A lista de recursos que se deseja filtrar as salas.
 * @param Aux A lista auxiliar para a recursão e usada como ponto de pártida. Deve ser inicializada como uma lista vazia.
 * @param List A lista de retorno contendo todas as salas que passaram o filtro, ou seja, todas aquelas que possuem os recursos exigidos.
 */
filterResources([], _, Aux, Aux).
filterResources([H|T], RequiredResources, Aux, List):-
    H = room(_, _, Resources, _, _, _),
    containsAllResources(Resources, RequiredResources),
    append(Aux, [H], Intermediate), filterResources(T, RequiredResources, Intermediate, List), !;
    filterResources(T, RequiredResources, Aux, List).

/**
 * searchRoomsResources(+RequiredResources:list, -Rooms:list) is det.
 * 
 * Recebe uma lista de recursos e retorna a lista com todas as salas que oferecem todos os recursos da lista e em quantidade suficiente.
 * @param RequiredResources A lista contendo os recursos que se deseja buscar.
 * @param Rooms A lista de retorno com todas as salas que possuem os recursos exigidos.
 */
searchRoomsResources(RequiredResources, Rooms) :-
    fetchRooms(Salas),
    filterResources(Salas, RequiredResources, [], Rooms).

/**
 * filterRequester(+Rooms:compound, +Requester:string, +Aux:list, -List:list) is det.
 * 
 * Filtro auxiliar que, dada uma lista de salas, retorna aquelas que possuem pelo menos uma reserva feita pelo usuário especificado.
 * @param Rooms A lista de salas a ser analisadas.
 * @param Requester O nome do usuário que se deve procurar entre os responsáveis por reservas de cada sala..
 * @param Aux Lista auxiliar para a recursão usada como ponto inicial. deve ser inicializada como uma lista vazia.
 * @param List Lista de retorno contendo todas as salas que passam o filtro, ou seja, aquelas que possuem alguma reserva feita pelo usuário especificado.
 */
filterRequester([], _, Aux, Aux).
filterRequester([H|T], Requester, Aux, List):-
    H = room(_, Schedule, _, _, _, _),
    wasReservedBy(Requester, Schedule),
    append(Aux, [H], Intermediate), filterRequester(T, Requester, Intermediate, List), !;
    filterRequester(T, Requester, Aux, List).

/**
 * hasRequester(+UserName:string, +Reservation:compound) is semidet.
 * 
 * Regra auxiliar. recebe um nome de usuário e uma reserva e sucede se a reserva foi feita pelo usuário.
 * @param UserName O nome do usuário.
 * @param Reservation A reserva verificada.
 */
hasRequester(UserName, Reservation) :-
    Reservation = reservation(UserName, _, _, _).

/**
 * wasReservedBy(+UserName:string, +Schedule:list) is semidet.
 * 
 * Regra auxiliar. Recebe um nome de usuário e uma lista de erservas e sucede se alguma das reservas da lista foi feita pelo usuário.
 * @param UserName O nome do usuário.
 * @param Schedule A lista de reservas que se deseja verificar.
 */
wasReservedBy(UserName, Schedule) :-
    include(hasRequester(UserName), Schedule, List),
    List \= [].

/**
 * searchRoomsRequester(+UserName:string, -Rooms:list) is det.
 * 
 * Recebe um nome de Usuário e retorna lista contendo todas as salas do sistema que possuem alguma reserva no nome deste usuário.
 * @param UserName O nome do usuário cujas salas reservadas se busca.
 * @param Rooms Lista de retorno contendo todas as salas que foram reservadas pelo usuário.
 */
searchRoomsRequester(UserName, Rooms) :-
    fetchRooms(Salas),
    filterRequester(Salas, UserName, [], Rooms).

/**
 * printCategories is det.
 * 
 * Imprime a lista de categorias de sala oferecidas pelo sistema.
 */
printCategories:-
    writeln("Qual categoria você deseja escolher?\n\c
            [L]aboratório\n\c
            [A]uditório\n\c
            [S]ala de aula\n\c
            [E]scritório\n\c
            [D]epósito").

/**
 * printResources is det.
 * 
 * Imprime a lista de todos os recursos oferecidos pelo sistema.
 */
printResources:-
    writeln("Qual recurso você deseja escolher?\n\c
            [P]rojetor\n\c
            [M]icroscópio\n\c
            [B]irô\n\c
            [C]omputador\n\c
            [Q]uadro\n\c
            [A]r condicionado").

/**
 * showResList(+ResourcesList:list, +Aux:string, -Text:string) is det.
 * 
 * Recebe uma lista de recursos e retorna a string repesentando-os.
 * @param ResourcesList Lista de recursos que se deseja representar em string.
 * @param Aux String auxiliar usada na recursão e usada como ponto de partida. Deve ser inicializada como string vazia.
 * @param Text A string representativa dos recursos da lista.
 */
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

/**
 * showRoom(+Room:compound, -Text:string) is det.
 * 
 * Recebe uma sala e retorna sua representação em string.
 * @param Room A sala.
 * @param a representação em string da sala.
 */
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

/**
 * showDate(+Date:compound, -Text:string) is det.
 * 
 * Recebe um date/9 e retorna sua representação em string.
 * @param Date O date/9 que se deseja formatar.
 * @param Text A representação string da data.
 */
showDate(Date, Text) :-
    Date = date(Y,M,D,H,Mn,S,_,_,_),
    number_string(Y, Year),
    number_string(M, Month),
    number_string(D, Day),
    number_string(H, Hour),
    number_string(Mn, Minute),
    number_string(S, Second),
    
    utils:stringBuilder([Day, "/", Month, "/", Year, " ", Hour, ":", Minute, ":", Second], "", Text).

/**
 * showReservation(+Reservation:compound, -Text:string) is det.
 * 
 * Recebe uma reserva e retorna a sua representação em string.
 * @param Reservation A reserva.
 * @param Text A representação em string da reserva.
 */
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

/**
 * showReservationList(+Schedule:list, +Aux:string, -Text:string) is det.
 * 
 * Recebe uma lista de reservas e cria a sua representação em string.
 * @param Schedule A lista de reservas que se deseja representar.
 * @param Aux String auxiliar usada na recursão e como ponto inicial. deve ser inicializada como string vazia.
 * @param Text A string de saída, com a representação textual da lista de reservas passada.
 */
showReservationList([], Aux, Aux).
showReservationList([H|T], Aux, Text) :-
    showReservation(H, Str),
    string_concat(Str, "\n-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n\n", Part),
    string_concat(Aux, Part, Intermediate),
    showReservationList(T, Intermediate, Text).

/**
 * filterReservations(+Schedule:list, +Day:compound, +Aux:list, -List:list) is det.
 * 
 * Filtro auxiliar que, dada uma lista de reservas e um dia, retorna as reservas que são para o dia especificado.
 * @param Schedule A lista de reservas a ser analisadas.
 * @param O date/3 contendo o dia que se deseja investigar.
 * @param Aux A lista auxiliar usada na recursão e como ponto inicial. deve ser inicializada como lista vazia.
 * @param List A lista de retorno, contendo todas as reservas que acontecem no dia especificado.
 */
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

/**
 * showDay(+Date:compound, -Text:string)
 * 
 * Recebe um date/3 e retorna a sua representação em string.
 * @param Date O date/3 que se deseja representar.
 * @param Text String de retorno contendo a representação da data.
 */
showDay(Date, Text) :-
    Date = date(Y,M,D),
    number_string(Y, Year),
    number_string(M, Month),
    number_string(D, Day),
    
    utils:stringBuilder([Day, "/", Month, "/", Year], "", Text).

/**
 * createReportsForTheRoom(+Day:compound, +Room:compound, -Text:string) is det.
 * 
 * Recebe uma sala e um dia e cria o relatório de ocupação daquela sala para o dia especificado.
 * @param Day O date/3 com o dia que se deseja investigar.
 * @param Room A sala cujo relatório se deseja criar.
 * @param Text String de retorno, contendo o relatório de ocupação da sala para o dia especificado.
 */
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

/**
 * createReports(+Day:compound, +Rooms:list, +Aux:string, -Text:string) is det.
 * 
 * Recebe um date/3, uma lista de salas e cria o relatório de ocupação para as salas na lista.
 * @param Day O date/3 com o dia que se deseja investigar.
 * @param Rooms A lista das salas cujos relatórios serão feitos.
 * @param Aux String auxiliar para a recursão e usada como ponto inicial. deve ser inicializada como uma string vazia.
 * @param Text String de saída com os relatórios de ocupação de cada sala da lista passada para o dia especificado.
 */
createReports(_, [], Aux, Aux).
createReports(Day, [H|T], Aux, Text):-
    createReportForTheRoom(Day, H, Line),
    string_concat(Aux, Line, Intermediate),

    createReports(Day, T, Intermediate, Text).

/**
 * createReportForTheDay(+Day:compound, -Text:String) is det.
 * 
 * Recebe uma data e retorna o relatório de ocupação das salas do sistema para este dia.
 * @param Day O date/3 contendo o dia pesquisado.
 * @param Text String de saída contendo os relatórios de ocupação.
 */
createReportForTheDay(Day, Text) :-
    fetchRooms(Rooms),
    createReports(Day, Rooms, "", Text).

/**
 * checkNewRoomCode(+CodeStr:string) is semidet.
 * 
 * Regra que recebe um código (não case-sensitive) de sala e sucede se este código não se referir a nenhuma sala do sistema.
 * @param CodeStr O código consultado.
 */
checkNewRoomCode(CodeStr):-
    string_upper(CodeStr, Code),
    dataHandler:readRooms,
    findall(_, dataHandler:room(Code, _, _, _, _, _), List),
    dataHandler:cleanRooms,
    List=[],!;
    errorHandler:promptError(9),fail.

/**
 * checkRoomCode(+CodeStr:string) is semidet.
 * 
 * Recebe um código (não case-sensitive) de sala e sucede se o código refere-se a uma das salas do sistema.
 * @param CodeStr O código consultado.
 */
checkRoomCode(CodeStr):-
    getRoom(CodeStr, _).