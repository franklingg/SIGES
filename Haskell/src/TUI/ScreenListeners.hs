{-|
Module      : ScreenListeners
Description : Módulo que estabelece ações e lógica das telas e de leitura de dados no sistema SIGES.
-}
module TUI.ScreenListeners(
    module TUI.ScreenListeners
) where

import qualified System.Console.Haskeline as Hkl
import qualified Data.Map as Map

import Manager
import TUI.OutputScreens
import Handlers.ErrorHandler
import Handlers.UserHandler
import Handlers.DataHandler
import Handlers.RoomsHandler

-- | Typeclass que estabelece as operações de ações a ser executadas em cada tela do sistema.
class Action a where
    useContent :: a -> IO Screen -- ^ A implementação de como a tela será impressa na tela e como lerá as informações dadas pelo usuário.

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
        screen <- getInputData getAnswer (validScreen ViewScreen)
        loggedUser <- hasLoggedUser
        return $ if screen == StartScreen && loggedUser then LoggedScreen else screen

    useContent ViewRoomScreen = do
        putStrLn $ getContent ViewRoomScreen
        putStrLn "Qual o código/nome da sala que você quer visualizar?"
        roomCode <- getInputData getAnswer checkRoomCode
        (Just room) <- getRoom roomCode
        print room
        putStrLn "Aperte qualquer tecla para continuar."
        getLine
        return StartScreen

    useContent ViewFilterScreen = do
        putStrLn $ getContent ViewFilterScreen
        rooms <- getRoomsFilter []
        sequence $ map print rooms
        return StartScreen

    useContent ReportRoomScreen = do
        putStrLn $ getContent ReportRoomScreen
        putStrLn "Qual o código/nome da sala que você quer visualizar?"
        roomCode <- getInputData getAnswer checkRoomCode
        (Just room) <- getRoom roomCode
        putStrLn "Qual o dia de ocupação você deseja ver [DD-MM-AAAA]?"
        [y,m,d] <- getInputData getAnswer checkDay
        putStrLn $ createReportForTheRoom (toInteger y, m, d) room
        waitInput
        return StartScreen

    useContent ReportDayScreen = do
        putStrLn $ getContent ReportDayScreen
        putStrLn "Qual o dia de ocupação você deseja ver [DD-MM-AAAA]?"
        [y,m,d] <- getInputData getAnswer checkDay
        report <- createReportForTheDay (toInteger y, m, d)
        putStrLn report
        waitInput
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
            then putStr "Reserva criada! "
            else putStr "Sala já ocupada neste horário. "
        waitInput
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
            then putStrLn "Reserva editada! "
            else putStrLn "A reserva não existe ou já está ocupada no novo horário. "
        waitInput
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
                putStrLn "Reserva deletada. "
                waitInput
                return())
        return LoggedScreen

-- | Esta é a função responsável por encontrar e controlar quais telas serão exibidas no sistema SIGES.
userInteraction :: Screen -> IO Screen
userInteraction screen = do
    firstAccess <- noUsersYet
    let currentScreen = if firstAccess then FirstScreen else screen

    nextScreen <- useContent currentScreen
    return nextScreen

waitInput :: IO String
waitInput = do
    putStrLn "Aperte qualquer tecla para continuar"
    getLine

-- | Esta função exibirá um prompt e captará a resposta digitada pelo usuário.
getAnswer :: IO String
getAnswer = do
    putStr ">> "
    hFlush (stdout)
    getLine

-- | Função responsável por captar a senha do usuário da entrada e garantir que ela seja exibida apenas com asteriscos, como forma de melhorar a segurança.
getPassword :: IO String
getPassword = Hkl.runInputT Hkl.defaultSettings $ do {p <- Hkl.getPassword (Just '*') ">> "; return $ fromJust p}

-- | Dada uma Mônada de String qualquer e uma função de verificação, esta função aplicará a função sobre a String. Caso a String passe na verificação fornecida, será retornada. Caso contrário, uma mensagem de erro será exibida e a função será repetida com uma nova entrada.
getInputData :: IO String -> (String -> IO (Either ErrorLog a)) -> IO a
getInputData getter checker = do
    input <- getter
    checkedAnswer <- checker input
    case checkedAnswer of
        Left error -> do {putStrLn error; getInputData getter checker}
        Right answer -> return answer

-- | Esta filtrará salas pelos filtros escolhidos pelo usuário. MAis de um filtro podem ser aplicados simultaneamente, neste caso, a função perguntará um a um quais filtros devem ser adicionados.
getRoomsFilter :: [Room] -> IO ([Room])
getRoomsFilter previous = do
    putStrLn "Por qual critério você deseja filtrar?\n\
              \1 - Categoria\n\
              \2 - Capacidade\n\
              \3 - Horário\n\
              \4 - Recursos"
    option <- getInputData getAnswer checkFilter
    rooms <- case option of 
                1 -> do 
                    printCategories 
                    cat <- getInputData getAnswer checkCategory
                    searchRoomsCategory $ Just cat
                2 -> do 
                    putStrLn "Capacidade mínima desejada: "
                    cap <- getInputData getAnswer checkNumber
                    searchRoomsCapacity $ Just cap
                3 -> do
                    putStrLn "Qual o dia a ser buscado [DD-MM-AAAA]?"
                    [y,m,d] <- getInputData getAnswer checkDay
                    putStrLn "Qual o horário de início a ser buscado [HH:MM]?"
                    [hStart, minStart] <- getInputData getAnswer checkTime
                    putStrLn "Qual o horário de término a ser buscado [HH:MM]?"
                    [hEnd, minEnd] <- getInputData getAnswer checkTime
                    searchRoomsTime (Just (toInteger y, m, d, hStart, minStart)) (Just (toInteger y,m,d,hEnd,minEnd))
                4 -> do
                    printResources
                    resources <- getResources []
                    searchRoomsResources $ Just resources

    let intersected = if previous == [] then rooms else rooms `intersect` previous
    putStrLn "Deseja combinar sua busca com outro filtro [S/N]?"
    more <- getInputData getAnswer yesOrNo
    if more then getRoomsFilter intersected else return intersected

-- | Função auxiliar à função de filtro de salas. Esta função lerá da entrada recursos que o usuário deseja buscar e suas respectivas quantidades, e então produzirá uma lista de recursos com estas informações e a retornará.
getResources :: [Resource] -> IO [Resource]
getResources previous = do
    kind <- getInputData getAnswer checkResource
    putStrLn "Qual a quantidade mínima?"
    quantity <- getInputData getAnswer checkNumber
    let resources = previous ++ [Resource kind quantity]
    putStrLn "Deseja buscar mais recursos [S/N]?"
    more <- getInputData getAnswer yesOrNo
    if more then getResources resources else return resources