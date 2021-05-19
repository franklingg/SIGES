/** <module> Listeners
*  Módulo que estabelece ações e lógica das telas e de leitura de dados no sistema SIGES.
*/
:- module(listeners, [screenListener/2]).
:- encoding(utf8).

:- use_module("./screens.pl").
:- use_module("./../utils.pl").
:- use_module("./../Handlers/dataHandler.pl").
:- use_module("./../Handlers/errorHandler.pl").
:- use_module("./../Handlers/userHandler.pl").
:- use_module("./../Handlers/roomsHandler.pl").

/**
 * screenListener(+T:string, -NextScreen: string) is nondet.
 * 
 * Dada a tela que está sendo exibida, realiza as operações equivalentes da mesma,
 * retornando a próxima sala a ser exibida.
 * @param T Tela a qual se refere o listener
 * @param NextScreen Próxima tela a ser exibida no sistema, escolhida durante a execução do listener
 */ 
screenListener('first', NextScreen):-
    writeln("Bem-vindo(a) ao SIGES! \n\c
             Vamos configurar o primeiro administrador do sistema."),
    writeln("Qual seu nome?"),
    promptString(">> ", Name),
    writeln("Qual seu e-mail?"),
    utils:getInputData(userHandler:checkValidEmail, Email),
    writeln("Qual sua senha?"),
    utils:getInputData(userHandler:checkValidPassword, Password),
    userHandler:createUser(Name, Email, Password, "s"),
    loggedUserScreen(NextScreen).

screenListener('start', _):- retractall(dataHandler:user).

screenListener('exit', _):- halt.

screenListener('login', NextScreen):-
    writeln("Qual seu e-mail?"),
    screen('login',_,N),
    utils:getInputData(userHandler:checkValidEmail, Email, N, NextScreen),
    (nonvar(NextScreen);
    writeln("Qual sua senha?"),
    utils:getInputData(userHandler:checkValidPassword, Password, N, NextScreen),
    (nonvar(NextScreen);
    userHandler:login(Email, Password),
    loggedUserScreen(NextScreen))).

screenListener('logged', _).

screenListener('admin', _).

screenListener('register_user', NextScreen):-
    writeln("Qual o nome do usuário a ser criado?"),
    utils:getInputData(userHandler:checkValidName, Name),
    writeln("Qual o e-mail do usuário a ser criado?"),
    utils:getInputData(userHandler:checkValidEmail, Email),
    writeln("Qual a senha do usuário a ser criado?"),
    utils:getInputData(userHandler:checkValidPassword, Password),
    writeln("O usuário é administrador [S/N]?"),
    utils:getYesOrNo(IsAdm),
    userHandler:createUser(Name, Email, Password, IsAdm),
    loggedUserScreen(NextScreen).

screenListener('delete_user', NextScreen):-
    writeln("Qual o e-mail do usuário a ser deletado?"),
    utils:getInputData(userHandler:checkValidEmail, Email),
    userHandler:deleteUser(Email),
    loggedUserScreen(NextScreen).

screenListener('add_new_room', NextScreen):-
    writeln("Qual o nome/código da sala a ser criada?"),
    utils:getInputData(roomsHandler:checkNewRoomCode, Code),
    roomsHandler:printCategories,
    utils:getInputData(dataHandler:checkCategory, Cat),
    dataHandler:getCategory(Cat, Category),
    getResources(Resources),
    writeln("Qual a capacidade da sala a ser criada?"),
    utils:getNumber(Capacity),
    writeln("Qual a localização da sala a ser criada?"),
    utils:getInputData(utils:trivial, Localization),
    roomsHandler:createRoom(Code, Resources, Category, Capacity, Localization),
    utils:waitInput("Sala criada! "),
    loggedUserScreen(NextScreen).

screenListener('view', NextScreen):-
    screen('view',_,NextScreens),
    utils:promptChoice(NextScreens, Next),
    (current_predicate(user/3),
    Next = 'start',
    loggedUserScreen(NextScreen),!;
    NextScreen = Next).

screenListener('view_user', NextScreen):-
    user(UserName, _, _),
    roomsHandler:searchRoomsRequester(UserName,Rooms),
    (Rooms=[], string_concat("Sem dados de salas encontrados para ", UserName, Str),writeln(Str),!;
    printRooms(Rooms)),
    writeln("\nDeseja visualizar de outras formas/alguma sala específica [S/N]?"),
    utils:getYesOrNo(Answer),
    Answer, NextScreen='view',!;
    loggedUserScreen(NextScreen).

screenListener('view_room', NextScreen):-
    writeln("Qual o código/nome da sala que você quer visualizar?"),
    utils:getInputData(roomsHandler:checkRoomCode, Code),
    roomsHandler:getRoom(Code, Room),
    printRooms([Room]),
    utils:waitInput,
    NextScreen='view'.

screenListener('view_filter', NextScreen):-
    getRoomsFilter([], Rooms),
    printRooms(Rooms),
    utils:waitInput,
    NextScreen='view'.

screenListener('report_room', NextScreen):-
    writeln("Qual o código/nome da sala que você quer visualizar?"),
    utils:getInputData(roomsHandler:checkRoomCode, Code),
    roomsHandler:getRoom(Code, Room),
    writeln("Qual o dia de ocupação você deseja ver [DD-MM-AAAA]?"),
    utils:getDate(D,M,Y),
    roomsHandler:createReportForTheRoom(date(Y,M,D), Room, Report),
    writeln(Report),
    utils:waitInput,
    NextScreen='view'.

screenListener('report_day', NextScreen):-
    writeln("Qual o dia de ocupação você deseja ver [DD-MM-AAAA]?"),
    utils:getDate(D,M,Y),
    roomsHandler:createReportForTheDay(date(Y,M,D), Report),
    writeln(Report),
    utils:waitInput,
    NextScreen='view'.

screenListener('make_reservation', NextScreen):-
    writeln("Qual código/nome da sala que você quer reservar?"),
    utils:getInputData(roomsHandler:checkRoomCode, Code),
    writeln("Qual o dia da reserva [DD-MM-AAAA]?"),
    utils:getDate(D, M, Y),
    writeln("Qual o horário de início [HH:MM]?"),
    utils:getTime(HourStart, MinuteStart),
    writeln("Qual o horário de término [HH:MM]?"),
    utils:getTime(HourEnd, MinuteEnd),
    Start=date(Y,M,D,HourStart,MinuteStart,0,10800,-,-),
    End=date(Y,M,D,HourEnd,MinuteEnd,0,10800,-,-),
    writeln("Dê uma breve descrição sobre a reserva"),
    utils:getInputData(utils:trivial, Description),
    dataHandler:user(Username, _, _),
    roomsHandler:makeReservation(Code, Username, Description, Start, End),
    utils:waitInput("Reserva criada! "),
    loggedUserScreen(NextScreen).


screenListener('edit_reservation', NextScreen):-
    writeln("Qual o código/nome da sala que você quer editar a reserva?"),
    utils:getInputData(roomsHandler:checkRoomCode, Code),
    writeln("Qual o dia da reserva feita [DD-MM-AAAA]?"),
    utils:getDate(DayOld, MonthOld, YearOld),
    writeln("Qual o horário de início da reserva feita [HH:MM]?"),
    utils:getTime(HourOld, MinuteOld),
    Old=date(YearOld,MonthOld,DayOld,HourOld,MinuteOld,0,10800,-,-),
    writeln("Qual o novo dia da reserva [DD-MM-AAAA]?"),
    utils:getDate(D, M, Y),
    writeln("Qual o novo horário de início da reserva [HH:MM]?"),
    utils:getTime(HourStart, MinuteStart),
    writeln("Qual o novo horário de término da reserva [HH:MM]?"),
    utils:getTime(HourEnd, MinuteEnd),
    NewStart=date(Y,M,D,HourStart,MinuteStart,0,10800,-,-),
    NewEnd=date(Y,M,D,HourEnd,MinuteEnd,0,10800,-,-),
    dataHandler:user(Username, _, _),
    roomsHandler:editReservation(Code, Username, Old, NewStart, NewEnd),
    utils:waitInput("Reserva editada! "),
    loggedUserScreen(NextScreen).

screenListener('remove_reservation', NextScreen):-
    writeln("Qual o código/nome da sala que você quer remover a reserva?"),
    utils:getInputData(roomsHandler:checkRoomCode, Code),
    writeln("Qual o dia da reserva feita [DD-MM-AAAA]?"),
    utils:getDate(D, M, Y),
    writeln("Qual o horário de início da reserva feita [HH:MM]?"),
    utils:getTime(HourStart, MinuteStart),
    Start=date(Y, M, D, HourStart, MinuteStart, 0, 10800, -, -),
    dataHandler:user(Username, _, _),
    roomsHandler:deleteReservation(Code, Username, Start),
    utils:waitInput("Reserva removida! "),
    loggedUserScreen(NextScreen).

/**
 * loggedUserScreen(-Next: string) is det.
 * 
 * Retorna a próxima tela de usuário logado, escolhendo entre tela de usuário comum ou administrador.
 * @param Next Próxima tela de usuário logado a ser exibida
 */ 
loggedUserScreen(Next):-
    dataHandler:user(_,_,IsAdm), 
    IsAdm=true->Next='admin';
    Next='logged'.

/**
 * getResources(-Resources:list) is multi.
 *
 * Recebe entradas do usuário e monta uma lista com recursos e suas respectivas quantidades,
 * de acordo com as entradas do usuário.
 * @param Resources Lista de recursos gerada pelas entradas do usuário
 */ 
getResources(Resources):-
    getResources([], Resources).

getResources(Aux, Resources):-
    roomsHandler:printResources,
    utils:getInputData(dataHandler:checkResource, K),
    dataHandler:getResource(K, Kind),
    writeln("Qual a quantidade desejada/existente do recurso?"),
    utils:getNumber(Amount),
    Resource = res(Kind, Amount),
    append(Aux, [Resource], NewAux),
    (writeln("Deseja buscar mais recursos [S/N]?"),
    utils:getYesOrNo(More),
    not(More), Resources=NewAux,!;
    getResources(NewAux, Resources)).

/**
 * checkFilter(+FilterStr: string) is semidet.
 * 
 * Dada uma string, checa se ela equivale a um dos possíveis filtros de visualização no SIGES.
 * @param FilterStr String a ser verificada
 */ 
checkFilter(FilterStr):-
    string_length(FilterStr, L), L = 1,
    member(FilterStr, ["1", "2", "3", "4"]),!;
    errorHandler:promptError(14), fail.

/**
 * getRoomsFilter(+Aux: string, Result: list) is multi.
 * 
 * Recebe entradas do usuário e monta uma lista de salas filtradas por um ou mais filtros de busca.
 * @param Aux Valor inicial da lista de salas (Recomenda-se se iniciar com [])
 * @param Result Resultado final da lista de salas após a busca por filtros
 */ 
getRoomsFilter(Aux, Result):-
    writeln('Por qual critério você deseja filtrar?\n\c
             1 - Categoria\n\c
             2 - Capacidade\n\c
             3 - Horário\n\c
             4 - Recursos'),
    utils:getInputData(listeners:checkFilter, Filter),
    (Filter="1" -> (
        roomsHandler:printCategories,
        utils:getInputData(dataHandler:checkCategory, Cat),
        dataHandler:getCategory(Cat, Category),
        roomsHandler:searchRoomsCategory(Category, Searched)
        ),!;
     Filter="2" -> (
        writeln("Qual a capacidade mínima desejada? "),
        utils:getNumber(Capacity),
        roomsHandler:searchRoomsCapacity(Capacity, Searched)
        ),!;
    Filter="3" -> (
        writeln("Qual o dia a ser buscado [DD-MM-AAAA]?"),
        utils:getDate(D,M,Y),
        writeln("Qual o horário de início a ser buscado [HH:MM]?"),
        utils:getTime(HourStart, MinStart),
        writeln("Qual o horário de término a ser buscado [HH:MM]?"),
        utils:getTime(HourEnd, MinEnd),
        StartTime=date(Y,M,D,HourStart,MinStart, 0, 10800,-,-),
        EndTime=date(Y,M,D,HourEnd,MinEnd, 0, 10800,-,-),
        roomsHandler:searchRoomsTime(StartTime, EndTime, Searched)
        ),!;
    Filter="4" -> (
        getResources(Resources),
        roomsHandler:searchRoomsResources(Resources, Searched)
        )
    ),
    (Aux=[], NewAux = Searched,!;
     intersection(Aux, Searched, NewAux)),
    writeln("Deseja combinar sua busca com outros filtros [S/N]?"),
    utils:getYesOrNo(More),
    (not(More), Result=NewAux,!;
    getRoomsFilter(NewAux, Result)).

/**
 * printRooms(+List:list) is det.
 *
 * Imprime o conteúdo de todas as salas recebidas na lista.
 * @param List Lista de salas a serem impressas 
 */ 
printRooms([]).
printRooms([Head|Tail]):-
    roomsHandler:showRoom(Head, Text),
    writeln(Text),
    printRooms(Tail).