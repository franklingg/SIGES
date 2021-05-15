:- encoding(utf8).

:- use_module("./../../src/utils.pl").

:- begin_tests(utils).

:- begin_tests(searchDict).

test(t1, [setup(promptTest("\n.Chave incorreta em dicionário - ")), fail]) :-
    D=_{"a":"sala a", "b": "sala b", "c": "sala c"},
    utils:searchDict(D, "bbb", _).

test(t2, [setup(promptTest("\n.Chave incorreta em dicionário - ")), fail]) :-
    D=_{"a":"sala a", "b": "sala b", "c": "sala c"},
    utils:searchDict(D, "d", _).

test(t3, [setup(promptTest("\n.Chave correta em dicionário - "))]) :-
    D=_{"a":"sala a", "b": "sala b", "c": "sala c"},
    utils:searchDict(D, "b", "sala b").

:- end_tests(searchDict).

:- begin_tests(yesOrNo).

test(t1, [setup(promptTest("\n.Mais de um caractere - ")), fail]) :-
    utils:yesOrNo("aa").

test(t2, [setup(promptTest("Caractere incorreto - ")), fail]) :-
    utils:yesOrNo("c").

test(t3, [setup(promptTest("Caractere correto - "))]) :-
    utils:yesOrNo("s"),
    utils:yesOrNo("n").

test(t4, [setup(promptTest("\n.Caractere correto e maiúsculo - "))]) :-
    utils:yesOrNo("S"),
    utils:yesOrNo("N").

:- end_tests(yesOrNo).

:- end_tests(utils).