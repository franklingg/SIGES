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
import Handlers.RoomsHandler

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
                return AdminScreen

    useContent StartScreen = do
        signOutUser
        cleanAllReservations
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
                signUser email
                return LoggedScreen
    
    useContent LoggedScreen  = do
        user <- getLoggedUser
        let userScreen = if isAdminUser user then AdminScreen else LoggedScreen
        putStrLn $ getContent userScreen
        getInputData getAnswer (validScreen userScreen)
    
    useContent AdminScreen = useContent LoggedScreen

    useContent SignUpScreen = do
        putStrLn $ getContent SignUpScreen
        putStrLn "Qual o e-mail do novo usuário?"
        email <- getInputData getAnswer checkNewEmail
        if email == "Retornar"
            then return LoggedScreen
            else do 
                putStrLn "Qual é a senha do novo usuário?"
                password <- getInputData getPassword checkNewPass
                putStrLn "Qual o nome do novo usuário?"
                name <- getInputData getAnswer checkName
                putStrLn "Este usuário é administrador (S/N)?"
                isAdm <- getInputData getAnswer yesOrNo
                registerNewUser email password name isAdm
                return $ LoggedScreen
    
    useContent RemoveUserScreen = do
        admUser <- getLoggedUser
        putStrLn $ getContent RemoveUserScreen
        putStrLn "Qual o e-mail do usuário a ser deletado?"
        email <- getInputData getAnswer checkEmail
        if email == "Retornar" then return ()
            else do 
                currentUser <- retrieveUser email
                putStrLn $ show currentUser
                putStrLn "Insira sua senha para confirmar:"
                admPassword <- getInputData getPassword $ checkPass (emailUser admUser)
                removeUser currentUser
                putStrLn "Usuário removido!"
        return LoggedScreen

    useContent ViewScreen = do
        putStrLn $ getContent ViewScreen
        getInputData getAnswer (validScreen ViewScreen)

    useContent ViewSpecificScreen = do
        putStrLn $ getContent ViewSpecificScreen
        putStrLn "Qual o código/nome da sala que você quer visualizar?"
        roomCode <- getInputData getAnswer checkRoomCode
        (Just room) <- getRoom roomCode
        putStrLn $ show room
        putStrLn "Deseja visualizar alguma reserva da sala acima [S/N]?"
        viewReservation <- getInputData getAnswer yesOrNo
        when viewReservation (do
                putStrLn "Qual o dia da reserva [DD-MM-AAAA]?"
                [y,m,d] <- getInputData getAnswer checkDay
                putStrLn "Qual o horário de início [HH:MM]?"
                [hStart, minStart] <- getInputData getAnswer checkTime
                reservation <- findReservationEasy roomCode (toInteger y,m,d,hStart,minStart)
                putStrLn $ show reservation)
        return StartScreen

    useContent CreateReservationScreen = do
        putStrLn $ getContent CreateReservationScreen
        putStrLn "Qual o código/nome da sala que você quer reservar?"
        roomCode <- getInputData getAnswer checkRoomCode
        putStrLn "Qual o dia da reserva [DD-MM-AAAA]?"
        [y,m,d] <- getInputData getAnswer checkDay
        putStrLn "Qual o horário de início [HH:MM]?"
        [hStart, mStart] <- getInputData getAnswer checkTime
        putStrLn "Qual o horário de término [HH:MM]?"
        [hEnd, mEnd] <- getInputData getAnswer checkTime
        putStrLn "Dê uma breve descrição sobre a reserva"
        description <- getInputData getAnswer checkDescription

        user <- getLoggedUser
        created <- makeReservation roomCode (nameUser user) description (toInteger y,m,d,hStart,mStart) (toInteger y,m,d,hEnd,mEnd)
        if created 
            then putStrLn "Reserva criada! Aperte qualquer tecla para continuar."
            else putStrLn "Sala já ocupada neste horário. Aperte qualquer tecla para continuar."
        getLine
        return LoggedScreen

    useContent EditReservationScreen = do
        putStrLn $ getContent EditReservationScreen
        putStrLn "Qual o código/nome da sala que você quer editar a reserva?"
        roomCode <- getInputData getAnswer checkRoomCode
        putStrLn "Qual o dia da reserva feita [DD-MM-AAAA]?"
        [yOld,mOld,dOld] <- getInputData getAnswer checkDay
        putStrLn "Qual o horário de início da reserva feita [HH:MM]?"
        [hOldStart, minOldStart] <- getInputData getAnswer checkTime
        putStrLn "Qual o novo dia da reserva [DD-MM-AAAA]?"
        [yNew,mNew,dNew] <- getInputData getAnswer checkDay
        putStrLn "Qual o novo horário de início da reserva [HH:MM]?"
        [hNewStart, minNewStart] <- getInputData getAnswer checkTime
        putStrLn "Qual o novo horário de término da reserva [HH:MM]?"
        [hNewEnd, minNewEnd] <- getInputData getAnswer checkTime

        user <- getLoggedUser
        edited <- editReservation roomCode (nameUser user) (toInteger yOld,mOld,dOld,hOldStart,minOldStart) (toInteger yNew,mNew,dNew,hNewStart,minNewStart) (toInteger yNew,mNew,dNew,hNewEnd,minNewEnd)
        if edited 
            then putStrLn "Reserva editada! Aperte qualquer tecla para continuar."
            else putStrLn "A reserva não existe ou já está ocupada no novo horário. Aperte qualquer tecla para continuar."
        getLine
        return LoggedScreen
    
    useContent RemoveReservationScreen = do
        putStrLn $ getContent RemoveReservationScreen
        putStrLn "Qual o código/nome da sala que você quer remover a reserva?"
        roomCode <- getInputData getAnswer checkRoomCode
        putStrLn "Qual o dia da reserva feita [DD-MM-AAAA]?"
        [y,m,d] <- getInputData getAnswer checkDay
        putStrLn "Qual o horário de início da reserva feita [HH:MM]?"
        [hStart, minStart] <- getInputData getAnswer checkTime

        user <- getLoggedUser
        reservation <- findReservation roomCode (toInteger y,m,d,hStart,minStart) (nameUser user)
        putStrLn $ show reservation
        putStrLn "Confirma a deleção da reserva acima [S/N]?"
        toDelete <- getInputData getAnswer yesOrNo
        when toDelete (do 
                deleteReservation roomCode (nameUser user) (toInteger y,m,d,hStart,minStart)
                putStrLn "Reserva deletada. Aperte qualquer tecla para continuar."
                getLine
                return())
        return LoggedScreen

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
