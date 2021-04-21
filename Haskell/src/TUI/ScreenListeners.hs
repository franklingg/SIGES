module TUI.ScreenListeners(
    module TUI.ScreenListeners
) where

-- importa Haskeline
import qualified System.Console.Haskeline as Hkl
-- importa Map
import qualified Data.Map as Map

-- importa a entidade Manager
import Manager
-- importa a entidade OutputScreens
import TUI.OutputScreens
-- importa a entidade ErrorHandler
import Handlers.ErrorHandler
-- importa a entidade UserHandler
import Handlers.UserHandler
-- importa a entidade DataHandler
import Handlers.DataHandler

{-
   Classe Action.
-}
class Action a where
    useContent :: a -> IO Screen

-- Action e uma instancia da classe de tipo Screen
instance Action Screen where
    useContent ExitScreen = do 
        putStrLn $ getContent ExitScreen    
        exitWith ExitSuccess

    useContent FirstScreen = do
        putStrLn $ getContent FirstScreen
        putStrLn "Qual o e-mail do novo usuário?"
        email <- getInputData getAnswer checkNewEmail
        if email == "Retornar"
            then return StartScreen
            else do 
                putStrLn "Qual é a senha do novo usuário?"
                password <- getInputData getPassword checkNewPass
                putStrLn "Qual o nome do novo usuário?"
                name <- getInputData getAnswer checkName
                registerNewUser email password name True
                currentUser <- retrieveUser email
                return $ AdminScreen currentUser

    useContent StartScreen = do
        putStrLn $ getContent StartScreen
        getInputData getAnswer (validScreen StartScreen)

    useContent LoginScreen = do
        putStrLn $ getContent LoginScreen
        putStrLn "Qual é o seu e-mail?"
        email <- getInputData getAnswer checkEmail
        if email == "Retornar"
            then return StartScreen
            else do 
                putStrLn "Qual é a sua senha?"
                password <- getInputData getPassword (checkPass email)
                user <- retrieveUser email
                return (LoggedScreen user)
    
    useContent (LoggedScreen user) = do
        if isAdminUser user then return (AdminScreen user) 
            else do
                let userScreen = (LoggedScreen user)
                putStrLn $ getContent userScreen
                getInputData getAnswer (validScreen userScreen)


    useContent (AdminScreen user) = do
        let admScreen = (AdminScreen user)
        putStrLn $ getContent admScreen
        getInputData getAnswer (validScreen admScreen)

    useContent (SignUpScreen user) = do
        putStrLn $ getContent (SignUpScreen user)
        putStrLn "Qual o e-mail do novo usuário?"
        email <- getInputData getAnswer checkNewEmail
        if email == "Retornar"
            then return (LoggedScreen user)
            else do 
                putStrLn "Qual é a senha do novo usuário?"
                password <- getInputData getPassword checkNewPass
                putStrLn "Qual o nome do novo usuário?"
                name <- getInputData getAnswer checkName
                putStrLn "Este usuário é administrador (S/N)?"
                isAdm <- getInputData getAnswer yesOrNo
                registerNewUser email password name isAdm
                currentUser <- retrieveUser email
                return $ (LoggedScreen user)
    
    useContent (RemoveUserScreen user) = do
        putStrLn $ getContent (RemoveUserScreen user)
        putStrLn "Qual o e-mail do usuário a ser deletado?"
        email <- getInputData getAnswer checkEmail
        if email == "Retornar" then return ()
            else do 
                currentUser <- retrieveUser email
                putStrLn $ show currentUser
                putStrLn "Insira sua senha para confirmar:"
                admPassword <- getInputData getPassword $ checkPass (emailUser user)
                removeUser currentUser
                putStrLn "Usuário removido!"
        return (LoggedScreen user)

{-
   Funcao para interacao com o usuario.
-}
userInteraction :: Screen -> IO Screen
userInteraction screen = do
    firstAccess <- noUsersYet
    let currentScreen = if firstAccess then FirstScreen else screen

    nextScreen <- useContent currentScreen
    return nextScreen

{-
   Funcao para obter resposta do usuario.
-}
getAnswer :: IO String
getAnswer = do
    putStr ">> "
    hFlush (stdout)
    getLine

{-
   Funcao para obter a senha do usuario.
-}
getPassword :: IO String
getPassword = Hkl.runInputT Hkl.defaultSettings $ do {p <- Hkl.getPassword (Just '*') ">> "; return $ fromJust p}

{-
   Funcao para obter os dados de entrada.
-}
getInputData :: IO String -> (String -> IO (Either ErrorLog a)) -> IO a
getInputData getter checker = do
    input <- getter
    checkedAnswer <- checker input
    case checkedAnswer of
        Left error -> do {putStrLn error; getInputData getter checker}
        Right answer -> return answer
