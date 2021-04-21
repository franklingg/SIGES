module TUI.OutputScreens
( module TUI.OutputScreens
) where

-- importa Map
import qualified Data.Map as Map
-- importa a entidade Manager
import Manager

{-
   Classe Content.
-}
class Content a where
    getContent :: a -> String
    nextScreens :: a -> Map.Map Char Screen

-- Content e uma instancia da classe de tipo Screen
instance Content Screen where
    getContent ExitScreen = "\nAté a próxima \n\
                             \e obrigado pelos peixes!\n"
    getContent FirstScreen = "\nBEM-VINDO(A) AO SIGES! PARA COMEÇAR, VAMOS CADASTRAR\n\
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
    getContent (LoggedScreen usr) ="\n\
                            \=========================\n\
                            \     TELA DE USUÁRIO    \n\
                            \=========================\n\
                            \[C]adastrar reservas      \n\
                            \[V]isualizar reservas    \n\
                            \[E]ditar reservas        \n\
                            \[D]eslogar"
    getContent (AdminScreen usr) = "\n\
                                    \==========================\n\
                                    \   TELA DE ADMINISTRADOR  \n\
                                    \==========================\n\
                                    \[C]adastrar reservas     \n\
                                    \[V]isualizar reservas    \n\
                                    \[A]lterar reservas        \n\
                                    \[R]egistrar novo usuário \n\
                                    \[E]xcluir usuário        \n\
                                    \[D]eslogar"
    getContent (SignUpScreen usr) = "\n\
                             \=========================\n\
                             \    CADASTRAR USUÁRIO    \n\
                             \=========================\n\
                             \[R]etornar (menu anterior)"
    getContent (RemoveUserScreen usr) = "\n\
                             \=========================\n\
                             \     REMOVER USUÁRIO     \n\
                             \=========================\n\
                             \[R]etornar (menu anterior)"

    nextScreens StartScreen = Map.fromList [('L', LoginScreen),('D', ExitScreen)]
    nextScreens LoginScreen = Map.fromList [('R', StartScreen)]
    nextScreens (LoggedScreen usr) = Map.fromList [('D', StartScreen)]
    nextScreens (AdminScreen usr) = Map.fromList [('R', (SignUpScreen usr)), ('E', (RemoveUserScreen usr)), ('D', StartScreen)]
    nextScreens _ = Map.empty