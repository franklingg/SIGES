:- encoding('utf8').

:- use_module("./../../src/Handlers/roomsHandler.pl").
:- use_module("./../../src/utils.pl", [promptTest/1]).

% Teste do Handler completo
salaZero(CodeStr, Schedule, Resources, Category, Capacity, Localization):-
    CodeStr = "SALAZERO",
    Schedule = [reservation("Patati", "Show", date(2015, 12, 17, 8, 0, 0, 10800, -, -), date(2015, 12, 17, 10, 0, 0, 10800, -, -)), reservation("Patati", "Show", date(2015, 12, 17, 12, 0, 0, 10800, -, -), date(2015, 12, 17, 14, 0, 0, 10800, -, -))],
    Resources = [],
    Category = dataHandler:category('Classroom'),
    Capacity = 100,
    Localization = "Bloco BL".
salaZero(R):-
    salaZero(CodeStr, Schedule, Resources, Category, Capacity, Localization),
    R=room(CodeStr, Schedule, Resources, Category, Capacity, Localization).

salaUm(CodeStr, Schedule, Resources, Category, Capacity, Localization):-
    CodeStr = "SALAUM",
    Schedule = [],
    Resources = [res(dataHandler:resourceKind('Microscope'), 5), res(dataHandler:resourceKind('Projector'), 1), res(dataHandler:resourceKind('Computer'), 1)],
    Category = dataHandler:category('Laboratory'),
    Capacity = 1,
    Localization = "Bloco BL".
salaUm(R):-
    salaUm(CodeStr, Schedule, Resources, Category, Capacity, Localization),
    R=room(CodeStr, Schedule, Resources, Category, Capacity, Localization).


:- begin_tests(roomsHandler).

:- begin_tests(createRoom).

test(t1, [setup(promptTest("\nCria salas corretamente: "))]):-
    salaZero(CodeStr, _, Res, Category, Capacity, Localization),
    roomsHandler:createRoom(CodeStr, Res, Category, Capacity, Localization).

test(t2, [setup(promptTest("\nCria salas corretamente: "))]):-
    salaUm(CodeStr, _, Res, Category, Capacity, Localization),
    roomsHandler:createRoom(CodeStr, Res, Category, Capacity, Localization).

test(t3, [setup(promptTest("\nAo tentar criar uma sala com código que existe: ")), fail]):-
    salaZero(CodeStr, _, Res, Category, Capacity, Localization),
    roomsHandler:createRoom(CodeStr, Res, Category, Capacity, Localization).

:- end_tests(createRoom).

:- begin_tests(getRoom).

test(t1, [setup(promptTest("\nAo se consultar uma sala normalmente:"))]):-
    salaZero(CodeStr, _, _, _, _, _),
    roomsHandler:getRoom(CodeStr, _).

test(t2, [setup(promptTest("\nAo se consultar uma sala com código errado: ")), fail]):-
    roomsHandler:getRoom("asdf", _).

:- end_tests(getRoom).

:- begin_tests(makeReservation).

test(t1, [setup(promptTest("\nAo se criar uma reserva nova: "))]):-
    roomsHandler:makeReservation("salazero", "Dida Ritz", "Performance", date(1995, 12, 17, 4, 0, 0, 10800, -, -), date(1995, 12, 17, 6, 30, 0, 10800, -, -)).
    
test(t2, [setup(promptTest("\nAo se criar uma reserva nova: "))]):-
    roomsHandler:makeReservation("salazero", "Patati", "Show", date(2005, 12, 17, 4, 0, 0, 10800, -, -), date(2005, 12, 17, 6, 30, 0, 10800, -, -)).

test(t3, [setup(promptTest("\nAo se criar uma reserva nova: "))]):-
    roomsHandler:makeReservation("salazero", "Patati", "Show", date(2015, 12, 17, 8, 0, 0, 10800, -, -), date(2015, 12, 17, 10, 0, 0, 10800, -, -)).

test(t4, [setup(promptTest("\nAo se criar uma reserva num horário ocupado: ")), fail]):-
    roomsHandler:makeReservation("salazero", "Princess", "Performing class", date(1995, 12, 17, 4, 0, 0, 10800, -, -), date(1995, 12, 17, 6, 30, 0, 10800, -, -)).
    
test(t5, [setup(promptTest("\nAo se criar uma reserva num horário parcialmente ocupado: ")), fail]):-
    roomsHandler:makeReservation("salazero", "Princess", "Performing class", date(1995, 12, 17, 5, 0, 0, 10800, -, -), date(1995, 12, 17, 6, 0, 0, 10800, -, -)).

test(t6, [setup(promptTest("\nAo se criar uma reserva num horário parcialmente ocupado: ")), fail]):-
    roomsHandler:makeReservation("salazero", "Princess", "Performing class", date(1995, 12, 17, 3, 0, 0, 10800, -, -), date(1995, 12, 17, 7, 4, 30, 10800, -, -)).

:- end_tests(makeReservation).

:- begin_tests(deleteReservation).

test(t1, [setup(promptTest("\nAo se deletar uma reserva válida: "))]):-
    roomsHandler:deleteReservation("salazero", "Dida Ritz", date(1995, 12, 17, 4, 0, 0, 10800, -, -)).

test(t2, [setup(promptTest("\nAo se deletar uma reserva válida com um username inválido: ")), fail]):-
    roomsHandler:deleteReservation("salazero", "Princess", date(1995, 12, 17, 4, 0, 0, 10800, -, -)).

test(t3, [setup(promptTest("\nAo se deletar uma reserva que não existe: ")), fail]):-
    roomsHandler:deleteReservation("salazero", "Princess", date(1995, 12, 17, 4, 0, 0, 10800, -, -)).

:- end_tests(deleteReservation).

:- begin_tests(editReservation).

test(t1, [setup(promptTest("\nAo se editar uma reserva que existe mas com um userName errado: ")), fail]):-
    roomsHandler:editReservation("salazero", "Wrong User", date(2005, 12, 17, 4, 0, 0, 10800, -, -), date(2015, 12, 17, 10, 0, 0, 10800, -, -), date(2015, 12, 17, 12, 0, 0, 10800, -, -)).

test(t2, [setup(promptTest("\nAo se editar uma reserva que existe com dados corretos, mas com um novo horário já ocupado: ")), fail]):-
    roomsHandler:editReservation("salazero", "Patati", date(2005, 12, 17, 4, 0, 0, 10800, -, -), date(2015, 12, 17, 8, 0, 0, 10800, -, -), date(2015, 12, 17, 10, 0, 0, 10800, -, -)).

test(t3, [setup(promptTest("\nAo se editar uma reserva que existe com dados corretos, mas com um novo horário parcialmente ocupado: ")), fail]):-
    roomsHandler:editReservation("salazero", "Patati", date(2005, 12, 17, 4, 0, 0, 10800, -, -), date(2015, 12, 17, 7, 0, 0, 10800, -, -), date(2015, 12, 17, 9, 0, 0, 10800, -, -)).

test(t4, [setup(promptTest("\nAo se editar uma reserva que existe com dados corretos: "))]):-
    roomsHandler:editReservation("salazero", "Patati", date(2005, 12, 17, 4, 0, 0, 10800,- , -), date(2015, 12, 17, 12, 0, 0, 10800, -, -), date(2015, 12, 17, 14, 0, 0, 10800, -, -)).

test(t5, [setup(promptTest("\nAo se editar uma reserva que não existe: ")), fail]):-
    roomsHandler:editReservation("salazero", "Patati", date(1995, 12, 17, 4, 0, 0, 10800, -, -), date(2015, 12, 17, 10, 0, 0, 10800, -, -), date(2015, 12, 17, 12, 0, 0, 10800, -, -)).

:- end_tests(editReservation).

:- begin_tests(searchRoomsCategory).

test(t1, [setup(promptTest("\nAo se buscar com a categoria adequada: "))]):-
    salaZero(R),
    roomsHandler:searchRoomsCategory(dataHandler:category('Classroom'), Rooms),
    Rooms = [R].

test(t2, [setup(promptTest("\nAo se buscar com a categoria sem salas: "))]):-
    roomsHandler:searchRoomsCategory(dataHandler:category('Auditorium'), Rooms),
    Rooms = [].

:- end_tests(searchRoomsCategory).

:-begin_tests(searchRoomsCapacity).

test(t1, [setup(promptTest("\nAo se buscar capacidade além da que qualquer sala pode oferecer: "))]):-
    roomsHandler:searchRoomsCapacity(1000, Rooms),
    Rooms = [].

test(t2, [setup(promptTest("\nAo se buscar capacidade abaixo da oferecida por todas as salas: "))]):-
    salaZero(R),
    salaUm(R1),
    roomsHandler:searchRoomsCapacity(1, Rooms),
    Rooms = [R1, R].

test(t3, [setup(promptTest("\nAo se buscar com uma capacidade qualquer: "))]):-
    salaZero(R),
    roomsHandler:searchRoomsCapacity(80, Rooms),
    Rooms = [R].

:- end_tests(searchRoomsCapacity).

:- begin_tests(searchRoomsTime).

test(t1, [setup(promptTest("\nAo se procurar num horário vago para todas as salas: "))]):-
    salaZero(R),
    salaUm(R1),
    roomsHandler:searchRoomsTime(date(2017, 4, 4, 12, 0, 0, 10800, -, -), date(2017, 4, 4, 14, 0, 0, 10800, -, -), Rooms),
    Rooms = [R1, R].

test(t2, [setup(promptTest("\nAo se procurar num horário qualquer: "))]):-
    salaUm(R1),
    roomsHandler:searchRoomsTime(date(2015, 12, 17, 10, 0, 0, 10800, -, -), date(2015, 12, 17, 13, 0, 0, 10800, -, -), Rooms),
    Rooms=[R1].

:- end_tests(searchRoomsTime).

:- begin_tests(searchRoomsResources).

test(t1, [setup(promptTest("\nAo se buscar um recurso que nenhuma sala possui: "))]):-
    roomsHandler:searchRoomsResources([res(dataHandler:resourceKind('Desk'), 1)], Rooms),
    Rooms = [].

test(t2, [setup(promptTest("\nAo se buscar um recurso em quantidade maior que a oferecida por qualquer sala: "))]):-
    roomsHandler:searchRoomsResources([res(dataHandler:resourceKind('Microscope'), 6)], Rooms),
    Rooms = [].

test(t3, [setup(promptTest("\nAo se buscar um recurso não oferecido com recursos oferecidos: "))]):-
    roomsHandler:searchRoomsResources([res(dataHandler:resourceKind('Microscope'), 5), res(dataHandler:resourceKind('Projector'), 1), res(dataHandler:resourceKind('Computer'), 1), res(dataHandler:resourceKind('Desk'), 1)], Rooms),
    Rooms = [].

test(t4, [setup(promptTest("\nAo se buscar com uma lista de recursos: "))]):-
    salaUm(R1),
    roomsHandler:searchRoomsResources([res(dataHandler:resourceKind('Microscope'), 5), res(dataHandler:resourceKind('Projector'), 1), res(dataHandler:resourceKind('Computer'), 1)], Rooms),
    Rooms = [R1].

:- end_tests(searchRoomsResources).

:- begin_tests(searchRoomsRequester).

test(t1, [setup(promptTest("\nAo se buscar salas de um usuário existente, mas sem reservas: "))]):-
    roomsHandler:searchRoomsRequester("Dida Ritz", Rooms),
    Rooms=[].

test(t2, [setup(promptTest("\nAo se buscar salas de um usuário existente, com reservas: "))]):-
    salaZero(R),
    roomsHandler:searchRoomsRequester("Patati", Rooms),
    Rooms=[R].

:- end_tests(searchRoomsRequester).

:- begin_tests(cleanAllReservations).

test(t1, [setup(promptTest("\nAo limpar as reservas: "))]):-
    roomsHandler:cleanAllReservations,
    getRoom("salazero", R),
    R = room(_, _, [], _, _, _).

:- end_tests(cleanAllReservations).

:- begin_tests(deleteRoom).

test(t1, [setup(promptTest("\nAo se deletar uma sala normalmente: "))]):-
    salaZero(CodeStr, _, _, _, _, _),
    roomsHandler:deleteRoom(CodeStr).

test(t2, [setup(promptTest("\nAo se deletar uma sala normalmente: "))]):-
    salaUm(CodeStr, _, _, _, _, _),
    roomsHandler:deleteRoom(CodeStr).

test(t3, [setup(promptTest("\nAo se deletar uma sala que não existe: ")), fail]):-
    salaZero(CodeStr, _, _, _, _, _),
    roomsHandler:deleteRoom(CodeStr).

:- end_tests(deleteRoom).

:- end_tests(roomsHandler).
