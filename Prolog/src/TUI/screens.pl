:- module(screens, [systemStart/1, screen/3]).
:- encoding(utf8).

:- use_module('./../Handlers/DataHandler.pl').

systemStart(T):-
        dataHandler:existsUserFile,
        T='start';
        T='first'.

screen('first',Content,_):-
        Content= '==========================\n\c
                \r      PRIMEIRO ACESSO     \n\c
                  =========================='.

screen('start',Content,NextScreens):-
    Content= '=========================\n\c
            \r   BEM-VINDO AO SIGES    \n\c
              =========================\n\c
               O que deseja fazer?     \n\c
              (L)ogin                  \n\c
              (V)isualizar ocupações   \n\c
              (D)esligar',
    NextScreens=_{'l':'login',
                  'v':'view',
                  'd':'exit'}.

screen('exit',Content,_):-
    Content= '\nAté mais\n\c
              e obrigado pelos peixes!\n'.

screen('login',Content,NextScreens):-
        Content= '======================\n\c
                \r   LOGIN DE USUÁRIO   \n\c
                  ======================\n\c
                  (R)etornar (menu principal)',
        NextScreens=_{'r':'start'}.

screen('logged',Content,NextScreens):-
        Content= '=====================\n\c
                \r   TELA DE USUÁRIO   \n\c
                  =====================\n\c
                  (C)adastrar reservas     \n\c
                  (V)isualizar reservas    \n\c
                  (E)ditar reservas        \n\c
                  (D)eslogar',
        NextScreens = _{'c':'make_reservation', 
                        'v':'view', 
                        'e':'edit_reservation', 
                        'd':'start'}.

screen('admin',Content,NextScreens):-
        Content="============================\n\c
               \r   TELA DE ADMINISTRADOR    \n\c
                 ============================\n\c
                 (C)adastrar reservas     \n\c
                 (V)isualizar reservas    \n\c
                 (A)lterar reservas       \n\c
                 (R)egistrar novo usuário \n\c
                 (E)xcluir usuário        \n\c
                 (D)eslogar",
        NextScreens = _{'c':'make_reservation', 
                        'v':'view', 
                        'a':'edit_reservation',
                        'r':'register_user',
                        'e':'delete_user', 
                        'd':'start'}.

screen('register_user',Content,_):-
        Content="=======================\n\c
                \r  CADASTRO DE USUÁRIO  \n\c
                 =======================".

screen('delete_user',Content,_):-
        Content="======================\n\c
                \r   DELETAR USUÁRIO   \n\c
                 ======================".

screen('view',Content,NextScreens):-
        Content= '====================\n\c
                \r   VISUALIZAÇÃO     \n\c
                  ====================\n\c
                  (R)etornar (menu principal)',
        NextScreens=_{'r':'start'}.