{-|
Module      : OutputScreens
Description : Módulo com o conteúdo exibido em cada tela e que armazena a relação entre as telas do sistema SIGES.
-}
module TUI.OutputScreens
( module TUI.OutputScreens
) where

import qualified Data.Map as Map
import Manager

-- | Typeclass Content, que estabelece operações de conteúdo para as telas do sistema.
class Content a where
    getContent :: a -> String               -- ^ Operação de obter o conteúdo de uma determinada tela.
    nextScreens :: a -> Map.Map Char Screen -- ^ Operação de obter as possíveis próximas telas de uma tela.

instance Content Screen where
    getContent ExitScreen = "\nAté a próxima \n\
                             \e obrigado pelos peixes!\n"
    getContent FirstScreen = "\nBEM-VINDO(A) AO SIGES! PARA COMEÇAR, VAMOS CADASRTRAR\n\
                              \O PRIMEIRO USUÁRIO (ADMINISTRADOR) DO SISTEMA!\n"
    getContent StartScreen ="\n=========================\n\
                            \  BEM-VINDO(A) AO SIGES  \n\
                            \=========================\n\
                            \\nO que deseja fazer?\n\
                            \[L]ogin\n\
                            \[V]isualizar reservas\n\
                            \[D]esligar"
    getContent LoginScreen ="\n\
                            \=========================\n\
                            \          LOGIN          \n\
                            \=========================\n\
                            \[R]etornar (menu inicial)"
    getContent LoggedScreen ="\n\
                            \=========================\n\
                            \     TELA DE USUÁRIO     \n\
                            \=========================\n\
                            \[C]adastrar reservas     \n\
                            \[V]isualizar reservas    \n\
                            \[E]ditar reservas        \n\
                            \[R]emover reservas       \n\   
                            \[D]eslogar"
    getContent AdminScreen = "\n\
                                    \==========================\n\
                                    \   TELA DE ADMINISTRADOR  \n\
                                    \==========================\n\
                                    \[C]adastrar reservas      \n\
                                    \[V]isualizar reservas     \n\
                                    \[A]lterar reservas        \n\
                                    \[D]eletar reservas        \n\
                                    \[R]egistrar novo usuário  \n\
                                    \[E]xcluir usuário         \n\
                                    \[F]azer logout"
    getContent SignUpScreen = "\n\
                             \=========================\n\
                             \    CADASTRAR USUÁRIO    \n\
                             \=========================\n\
                             \[R]etornar (menu anterior)"
    getContent RemoveUserScreen = "\n\
                             \=========================\n\
                             \     REMOVER USUÁRIO     \n\
                             \=========================\n\
                             \[R]etornar (menu anterior)"

    getContent ViewScreen = "\n\
                             \==========================    \n\
                             \  VISUALIZAÇÃO DE SALAS       \n\
                             \==========================    \n\
                             \[D]ados de uma sala específica\n\
                             \[F]iltrar salas por atributo  \n\
                             \[O]cupação por sala           \n\
                             \[S]alas ocupadas em certo dia \n\   
                             \[R]etornar (menu principal)"
    getContent ViewRoomScreen = "\n\
                            \=============================\n\
                            \  VISUALIZAR SALA ESPECÍFICA   \n\
                            \=============================\n"
    getContent ViewFilterScreen = "\n\
                            \==========================\n\
                            \  VISUALIZAR POR FILTROS    \n\
                            \==========================\n"
    getContent ReportRoomScreen = "\n\
                            \=============================\n\
                            \ RELATÓRIO DE SALA ESPECÍFICA\n\
                            \=============================\n"
    getContent ReportDayScreen = "\n\
                            \=============================\n\
                            \ RELATÓRIO DE DIA ESPECÍFICO \n\
                            \=============================\n"
    getContent CreateReservationScreen = "\n\
                             \=========================\n\
                             \    CADASTRAR RESERVAS   \n\
                             \=========================\n"
    getContent EditReservationScreen = "\n\
                             \=========================\n\
                             \     EDITAR RESERVAS     \n\
                             \=========================\n"
    getContent RemoveReservationScreen = "\n\
                            \=========================\n\
                            \     REMOVER RESERVAS    \n\
                            \=========================\n"

    nextScreens StartScreen = Map.fromList [('L', LoginScreen),('V', ViewScreen), ('D', ExitScreen)]
    nextScreens LoginScreen = Map.fromList [('R', StartScreen)]
    nextScreens LoggedScreen = Map.fromList [('C', CreateReservationScreen), ('V', ViewScreen), ('E',EditReservationScreen), ('R', RemoveReservationScreen), ('D', StartScreen)]
    nextScreens AdminScreen = Map.fromList [('C', CreateReservationScreen), ('V', ViewScreen), ('A',EditReservationScreen), ('D',RemoveReservationScreen),('R', SignUpScreen), ('E', RemoveUserScreen), ('F', StartScreen)]
    nextScreens SignUpScreen = Map.fromList [('R', StartScreen)]
    nextScreens RemoveUserScreen = Map.fromList [('R', StartScreen)]
    nextScreens ViewScreen = Map.fromList [('D', ViewRoomScreen), ('F',ViewFilterScreen), ('O',ReportRoomScreen),('S',ReportDayScreen), ('R', StartScreen)]
    nextScreens _ = Map.empty