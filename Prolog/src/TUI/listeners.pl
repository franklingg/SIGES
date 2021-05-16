:- module(listeners, [screenListener/2]).
:- encoding(utf8).

:- use_module("./screens.pl").
:- use_module("./../utils.pl").
:- use_module("./../Handlers/dataHandler.pl").
:- use_module("./../Handlers/userHandler.pl").
:- use_module("./../Handlers/roomsHandler.pl").

screenListener('first', NextScreen):-
    writeln("Bem-vindo(a) ao SIGES! \n\c
             Vamos configurar o primeiro administrador do sistema."),
    writeln("Qual seu nome?"),
    promptString(">> ", Name),
    utils:emptyDict(N),
    writeln("Qual seu e-mail?"),
    utils:getInputData(userHandler:checkValidEmail, Email, N, _),
    writeln("Qual sua senha?"),
    utils:getInputData(userHandler:checkValidPassword, Password, N, _),
    userHandler:createUser(Name, Email, Password, "s"),
    loggedUserScreen(NextScreen).

screenListener('start', _).

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
    utils:emptyDict(N),
    utils:getInputData(userHandler:checkValidName, Name, N, _),
    writeln("Qual o e-mail do usuário a ser criado?"),
    utils:getInputData(userHandler:checkValidEmail, Email, N, _),
    writeln("Qual a senha do usuário a ser criado?"),
    utils:getInputData(userHandler:checkValidPassword, Password, N, _),
    writeln("O usuário é administrador [S/N]?"),
    utils:getInputData(utils:yesOrNo, IsAdm, N, _),
    userHandler:createUser(Name, Email, Password, IsAdm),
    loggedUserScreen(NextScreen).

screenListener('delete_user', NextScreen):-
    utils:emptyDict(N),
    writeln("Qual o e-mail do usuário a ser deletado?"),
    utils:getInputData(userHandler:checkValidEmail, Email, N, _),
    userHandler:deleteUser(Email),
    loggedUserScreen(NextScreen).

screenListener('add_new_room', NextScreen):-
    utils:emptyDict(N),
    writeln("Qual o nome/código da sala a ser criada?"),
    utils:getInputData(roomsHandler:checkNewRoomCode, Code, N, _),
    roomsHandler:printCategories,
    utils:getInputData(dataHandler:checkCategory, Cat, N, _),
    dataHandler:getCategory(Cat, Category),
    getResources(Resources),
    writeln("Qual a capacidade da sala a ser criada?"),
    utils:getInputData(utils:checkNumber, Cap, N, _),
    number_string(Capacity, Cap),
    writeln("Qual a localização da sala a ser criada?"),
    utils:getInputData(utils:trivial, Localization, N, _),
    roomsHandler:createRoom(Code, Resources, Category, Capacity, Localization),
    waitInput("Sala criada! "),
    loggedUserScreen(NextScreen).

screenListener('view', _).

screenListener('view_user_screen', _).

screenListener('view_room_screen', _).

screenListener('view_filter_screen', _).

screenListener('report_room_screen', _).

screenListener('report_day_screen', _).

screenListener('make_reservation', _).

screenListener('edit_reservation', _).

screenListener('remove_reservation', _).


loggedUserScreen(Next):-
    dataHandler:user(_,_,IsAdm), 
    IsAdm=true->Next='admin';
    Next='logged'.

getResources(Resources):-
    getResources([], Resources).

getResources(Aux, Resources):-
    utils:emptyDict(N),
    roomsHandler:printResources,
    utils:getInputData(dataHandler:checkResource, K, N, _),
    dataHandler:getResource(K, Kind),
    writeln("Qual a quantidade desejada/existente do recurso?"),
    utils:getInputData(utils:checkNumber, A, N, _),
    number_string(Amount, A),
    Resource = res(Kind, Amount),
    append(Aux, [Resource], I),
    writeln("Deseja buscar mais recursos [S/N]?"),
    utils:getInputData(utils:yesOrNo, M, N, _),
    string_upper(M, More),
    More="N",Resources=I,!;
    getResources(I, Resources).

