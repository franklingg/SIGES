:- encoding(utf8).

:- use_module("./../../src/Handlers/userHandler.pl").
:- use_module("./../../src/utils.pl", [promptTest/1]).

userTest(N, E, P, A):-
    N="userTest",
    E="email@test.com",
    P="passwordTest",
    A="n".

:- begin_tests(userHandler).

:- begin_tests(checkValidEmail).

test(t1, [setup(promptTest("\n.Email sem @ - ")), fail]) :-
    userHandler:checkValidEmail("fulanofulano.com").

test(t2, [setup(promptTest("Email sem dominio - ")), fail]) :-
    userHandler:checkValidEmail("fulano@").

test(t3, [setup(promptTest("Email sem nome - ")), fail]) :-
    userHandler:checkValidEmail("@fulano.com").

test(t4, [setup(promptTest("Email correto - "))]) :- 
    userHandler:checkValidEmail("fulano@fulano.com").

:- end_tests(checkValidEmail).

:- begin_tests(checkValidPassword).

test(t1, [setup(promptTest("\n.Senha curta - ")), fail]) :- 
    userHandler:checkValidPassword("1234567").

test(t2, [setup(promptTest("Senha Boa - "))]) :- 
    userHandler:checkValidPassword("12345678").

:- end_tests(checkValidPassword).

:- begin_tests(checkValidName).

test(t1, [setup(promptTest("\n.Nome curto - ")), fail]) :- 
    userHandler:checkValidName("a").

test(t2, [setup(promptTest("Nome OK - "))]) :- 
    userHandler:checkValidName("Jose").

:- end_tests(checkValidName).


:- begin_tests(createUser).

test(t1, [setup(promptTest("\n.Cria usuário corretamente - "))]) :-
    userTest(Name, Email, Password, AdmChar),
    userHandler:createUser(Name, Email, Password, AdmChar).

test(t2, [setup(promptTest("\n.Usuário já existe - ")), fail]) :-
    userTest(Name, Email, Password, AdmChar),
    userHandler:createUser(Name, Email, Password, AdmChar).

:- end_tests(createUser).

:- begin_tests(login).

test(t1, [setup(promptTest("\n.Faz login corretamente - "))]) :-
    userTest(_, Email, Password, _),
    userHandler:login(Email, Password).

test(t2, [setup(promptTest("\n.Email incorreto - ")), fail]) :-
    userTest(_, _, Password, _),
    userHandler:login("incorrect@email.com", Password).

test(t3, [setup(promptTest("Senha incorreta - ")), fail]) :-
    userTest(_, Email, _, _),
    userHandler:login(Email, "senhaincorreta").

:- end_tests(login).

:- begin_tests(deleteUser).

test(t1, [setup(promptTest("\n.Deleta usuário corretamente - "))]) :-
    userTest(_, Email, _, _),
    userHandler:deleteUser(Email).

test(t2, [setup(promptTest("\n.Usuário não existe - ")), fail]) :-
    userHandler:deleteUser("email@inexistente.com").

:- end_tests(deleteUser).

:- end_tests(userHandler).