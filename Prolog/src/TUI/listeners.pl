:- module(listeners, [screenListener/2]).
:- encoding(utf8).

:- use_module('./screens.pl').
:- use_module('./../utils.pl').
:- use_module('./../Handlers/dataHandler.pl').
:- use_module('./../Handlers/userHandler.pl').

screenListener('first', NextScreen):-
    writeln("Bem-vindo(a) ao SIGES! \n\c
             Vamos configurar o primeiro administrador do sistema."),
    writeln('Qual seu nome?'),
    promptString('>> ', Name),
    utils:emptyDict(N),
    writeln('Qual seu e-mail?'),
    utils:getInputData(userHandler:checkValidEmail, Email, N, _),
    writeln('Qual sua senha?'),
    utils:getInputData(userHandler:checkValidPassword, Password, N, _),
    userHandler:createUser(Name, Email, Password, 's'),
    loggedUserScreen(NextScreen).

screenListener('start', _).

screenListener('exit', _):- halt.

screenListener('login', NextScreen):-
    writeln('Qual seu e-mail?'),
    screen('login',_,N),
    utils:getInputData(userHandler:checkValidEmail, Email, N, NextScreen),
    (nonvar(NextScreen);
    writeln('Qual sua senha?'),
    utils:getInputData(userHandler:checkValidPassword, Password, N, NextScreen),
    (nonvar(NextScreen);
    userHandler:login(Email, Password),
    loggedUserScreen(NextScreen);
    NextScreen='login')).

screenListener('logged', _).

screenListener('admin', _).

screenListener('register_user', NextScreen):-
    writeln('Qual o nome do usuário a ser criado?'),
    utils:emptyDict(N),
    utils:getInputData(userHandler:checkValidName, Name, N, _),
    writeln('Qual o e-mail do usuário a ser criado?'),
    utils:getInputData(userHandler:checkValidEmail, Email, N, _),
    writeln('Qual a senha do usuário a ser criado?'),
    utils:getInputData(userHandler:checkValidPassword, Password, N, _),
    writeln('O usuário é administrador [S/N]?'),
    utils:getInputData(utils:yesOrNo, IsAdm, N, _),
    userHandler:createUser(Name, Email, Password, IsAdm),
    loggedUserScreen(NextScreen);
    NextScreen = 'register_user'.

screenListener('delete_user', NextScreen):-
    utils:emptyDict(N),
    writeln('Qual o e-mail do usuário a ser deletado?'),
    utils:getInputData(userHandler:checkValidEmail, Email, N, _),
    userHandler:deleteUser(Email),
    loggedUserScreen(NextScreen);
    NextScreen = 'delete_user'.

screenListener('view', _).


loggedUserScreen(Next):-
    dataHandler:user(_,_,IsAdm), 
    IsAdm=true->Next='admin';
    Next='logged'.