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
                  (D)eletar reservas      \n\c
                  (F)azer logout',
        NextScreens = _{'c':'make_reservation', 
                        'v':'view_user_screen', 
                        'e':'edit_reservation', 
                        'd':'remove_reservation',
                        'f':'start'}.

screen('admin',Content,NextScreens):-
        Content='============================\n\c
               \r   TELA DE ADMINISTRADOR    \n\c
                 ============================\n\c
                 (I)ncluir novas salas    \n\c
                 (C)adastrar reservas     \n\c
                 (V)isualizar reservas    \n\c
                 (A)lterar reservas       \n\c
                 (D)eletar reservas      \n\c
                 (R)egistrar novo usuário \n\c
                 (E)xcluir usuário        \n\c
                 (F)azer Logout',
        NextScreens = _{'i': 'add_new_room',
                        'c':'make_reservation', 
                        'v':'view_user_screen', 
                        'a':'edit_reservation',
                        'd':'remove_reservation',
                        'r':'register_user',
                        'e':'delete_user', 
                        'f':'start'}.

screen('register_user',Content,_):-
        Content='=======================\n\c
                \r  CADASTRO DE USUÁRIO  \n\c
                 ======================='.

screen('delete_user',Content,_):-
        Content='======================\n\c
                \r   DELETAR USUÁRIO   \n\c
                 ======================'.

screen('view',Content,NextScreens):-
        Content= '=======================\n\c
                \r VISUALIZAÇÃO DE SALAS \n\c
                  =======================\n\c
                  (D)ados de uma sala específica \n\c
                  (F)iltrar salas por atributo   \n\c
                  (O)cupação por sala            \n\c
                  (S)alas ocupadas em certo dia  \n\c 
                  (R)etornar (menu principal)',
        NextScreens=_{'d':'view_room_screen',
                      'f':'view_filter_screen',
                      'o':'report_room_screen',
                      's':'report_day_screen',
                      'r':'start'}.

screen('view_user_screen',Content,_):-
        Content='=============================\n\c
                \r VISUALIZAR SALAS DE USUÁRIO \n\c
                 ============================='.

screen('view_room_screen',Content,_):-
        Content='=============================\n\c
                \r VISUALIZAR SALA ESPECÍFICA \n\c
                 ============================='.

screen('view_filter_screen',Content,_):-
        Content='=========================\n\c
                \r VISUALIZAR POR FILTROS \n\c
                 ========================='.

screen('report_room_screen',Content,_):-
        Content='===============================\n\c
                \r RELATÓRIO DE SALA ESPECÍFICA \n\c
                 ==============================='.

screen('report_day_screen',Content,_):-
        Content='==============================\n\c
                \r RELATÓRIO DE DIA ESPECÍFICO \n\c
                 =============================='.

screen('add_new_room',Content,_):-
        Content='=======================\n\c
                \r INCLUIR NOVAS SALAS  \n\c
                 ======================='.

screen('make_reservation',Content,_):-
        Content='=======================\n\c
                \r  CADASTRAR RESERVA   \n\c
                 ======================='.

screen('edit_reservation',Content,_):-
        Content='===================\n\c
                \r  EDITAR RESERVA   \n\c
                 ==================='.

screen('remove_reservation',Content,_):-
        Content='=====================\n\c
                \r  REMOVER RESERVA   \n\c
                 ====================='.