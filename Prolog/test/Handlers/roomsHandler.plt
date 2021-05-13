:- encoding('utf8').

:- use_module('./../../src/Handlers/roomsHandler.pl').
:- use_module('./../../src/utils.pl', [promptTest/1]).

% Teste do Handler completo
:- begin_tests(roomsHandler).

roomTest(CodeStr, Resources, Category, Capacity, Localization):-
    CodeStr = 'caa-1',
    Resources = [],
    Category = category('Classroom'),
    Capacity = 99,
    Localization = 'Bloco CAA'.


% Testes de cada método a ser testado
% Trocar FUNCAO pelo nome da funcao a ser testada
%:- begin_tests(FUNCAO).
:- begin_tests(createRoom).

% Teste de uma funcionalidade específica
% Trocar STRING pela mensagem de teste. Ex: "Email sem @ - ", "Sala com ID incorreto - "
% Se for testar um caso de sucesso, retira o fail
%test(t1, [setup(promptTest(STRING)), fail]) :-
%    BODY DO TESTE
test(t1, [setup(promptTest('\nCria sala corretamente:'))]):-
    roomTest(CodeStr, Resources, Category, Capacity, Localization),
    roomsHandler:createRoom(CodeStr, Resources, Category, Capacity, Localization),!.

test(t2, [setup(promptTest('\nAo tentar criar uma sala com código que existe:')), fail]):-
    roomTest(CodeStr, Resources, Category, Capacity, Localization),
    roomsHandler:createRoom(CodeStr, Resources, Category, Capacity, Localization).

%:- end_tests(FUNCAO).
:- end_tests(createRoom).

:- begin_tests(getRoom).

test(t1, [setup(promptTest('\nAo se consultar uma sala normalmente:'))]):-
    roomTest(CodeStr, _, _, _, _),
    roomsHandler:getRoom(CodeStr, _).

test(t2, [setup(promptTest('\nAo se consultar uma sala com código errado: ')), fail]):-
    roomsHandler:getRoom('asdf', _).

test(t3, [setup(promptTest('\nAo se consultar uma sala com o codigo em caixa alta: '))]):-
    roomTest(CodeStr, _, _, _, _),
    string_upper(CodeStr, Code),
    roomsHandler:getRoom(Code, _).

:- end_tests(getRoom).

:- begin_tests(deleteRoom).

test(t1, [setup(promptTest('\nAo se deletar uma sala normalmente: '))]):-
    roomTest(CodeStr, _, _, _, _),
    roomsHandler:deleteRoom(CodeStr).

test(t2, [setup(promptTest('\nAo se deletar uma sala que não existe: ')), fail]):-
    roomTest(CodeStr, _, _, _, _),
    roomsHandler:deleteRoom(CodeStr).

:- end_tests(deleteRoom).



:- end_tests(roomsHandler).
