{-|
Module      : RoomsHandler
Description : Módulo contendo as operações básicas para manipulação de salas no sistema SIGES.
-}
module Handlers.RoomsHandler where

import Manager
import Handlers.DataHandler

instance Show Room where
    show(Room codeRoom _ resRoom capRoom localRoom catRoom) =   "Dados da sala:                   \n\
                                                                \Código: "++ codeRoom ++         "\n\
                                                                \Categoria: "++ show catRoom ++  "\n\
                                                                \Capacidade: "++ show capRoom ++ "\n\
                                                                \Localização: "++ localRoom ++   "\n\
                                                                \Recursos: "++ show resRoom

instance Show Reservation where
    show(Reservation requester description startTime finishTime) = "Dados da reserva:                 \n\
                                                                   \Início: "++ show startTime ++    "\n\
                                                                   \Fim: "++ show finishTime ++      "\n\
                                                                   \Responsável: " ++ requester ++   "\n\
                                                                   \Motivo: "++ description
                                                                
instance Ord Reservation where
    compare res1 res2 = if startTime res1 <= startTime res2 then LT else GT

instance Show Resource where
    show(Resource kind quantity) = " " ++ show kind ++ ": " ++ show quantity

-- | Função auxiliar que, dada uma tupla com um Integer para o ano e um Int para mÊs, dia, hora e minuto, respectivamente, cria um LocalTime.
makeTime :: (Integer, Int, Int, Int, Int) -> LocalTime
makeTime (a, m, d, h, min) = time
    where (Just clock) = makeTimeOfDayValid h min 00
          calendar = fromGregorian a m d
          time = LocalTime calendar clock

-- | Esta função recebe uma sala e um horário, e verifica se esta sala estará livre neste horário, respondendo com um valor booleano.
isFree :: Room -> LocalTime -> Bool
isFree room newTime = not $ any (\reservation -> (startTime reservation <= newTime) && (finishTime reservation >= newTime)) (schedule room)

-- | Esta função criará uma reserva em uma das salas, a partir do código da mesma, do nome do responsável pela reserva, e das tuplas especificando data e horário de finalização do evento.
makeReservation :: String -> String -> String -> (Integer, Int, Int, Int, Int) -> (Integer, Int, Int, Int, Int) -> IO Bool
makeReservation codeStr userName descriptionStr startTimeTuple finishTimeTuple = do
    let codeRoom = map toUpper codeStr
        startTimeReservation = makeTime startTimeTuple
        finishTimeReservation = makeTime finishTimeTuple
    (Just room) <- getRoom codeRoom
    
    if isFree room startTimeReservation && isFree room finishTimeReservation 
        then do
            let newReservation = Reservation{requester=userName, description=descriptionStr, startTime=startTimeReservation, finishTime=finishTimeReservation}
                newSchedule = insert newReservation (schedule room)
                newRoom = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category =category room}
            
            updateRoom codeRoom newRoom
            return True
        else
            return False

-- | Esta função deletará uma função identificada pelo código da sala e nome do responsável, e pela tupla do horário de início do evento.
deleteReservation :: String -> String -> (Integer, Int, Int, Int, Int) -> IO Bool
deleteReservation codeStr userName startTimeTuple = do
    let codeRoom = map toUpper codeStr
        timeReservation = makeTime startTimeTuple
    (Just room) <- getRoom codeRoom
    
    if any (\reservation -> (startTime reservation == timeReservation) && (requester reservation == userName)) (schedule room)
        then do 
            let newSchedule = filter (\reservation -> startTime reservation /= timeReservation) (schedule room)
                newRoom = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category=category room}
            
            updateRoom codeRoom newRoom
            return True

        else return False

-- | Dada uma sala, identificada pelo seu código, um horário, representado em forma de tupla e um nome de usuário, esta função retornará a reserva com os dados equivalentes.
findReservation :: String -> (Integer,Int,Int,Int,Int)-> String -> IO Reservation
findReservation codeRoom startTimeTuple userName = do
    (Just room) <- getRoom codeRoom
    let reservations = schedule room
        matching = filter (\reservation -> (startTime reservation == makeTime startTimeTuple) && (requester reservation == userName)) reservations
    return $ head matching

-- | Dada uma sala, identificada pelo seu código, e um horário em forma de tupla, esta função retornará a reserva da sala cujo evento se inicie no horário dado.
findReservationEasy :: String -> (Integer,Int,Int,Int,Int) -> IO Reservation
findReservationEasy codeRoom startTimeTuple = do
    (Just room) <- getRoom codeRoom
    let reservations = schedule room
        matching = filter (\reservation -> (startTime reservation == makeTime startTimeTuple)) reservations
    return $ head matching


-- | Esta função alterará o horário de uma reserva (identificada pelo código de sua sala, pelo seu responsável e horário de início). Caso não seja possível fazer a alteração, nada será feito. A função retornará um valor booleano indicando se foi possível fazer a operação.
editReservation :: String -> String -> (Integer, Int, Int, Int, Int) -> (Integer, Int, Int, Int, Int) -> (Integer, Int, Int, Int, Int) -> IO Bool
editReservation codeStr userName currentStartTimeTuple newStartTimeTuple newFinishTimeTuple = do
    let codeRoom = map toUpper codeStr    
    (Just room) <- getRoom codeRoom
    let currentStartTime = makeTime currentStartTimeTuple
        newStartTime = makeTime newStartTimeTuple
        newFinishTime = makeTime newFinishTimeTuple
        reservationExists = any (\reservation -> startTime reservation == currentStartTime && requester reservation == userName) (schedule room)
        newTimeIsFree = isFree room newStartTime && isFree room newFinishTime

    if reservationExists && newTimeIsFree
        then do
            let currentReservation = head $ filter (\reservation -> startTime reservation == currentStartTime) (schedule room)
                newReservation = Reservation{requester = userName, description = description currentReservation, startTime = newStartTime, finishTime = newFinishTime}
                newSchedule = insert newReservation (filter (\reservation -> startTime reservation /= currentStartTime) (schedule room))
                newRoom = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category=category room}
            
            updateRoom codeRoom newRoom
            return True

        else do
            return False


-- | Dado um LocalTime para comparação, esta função irá retirar de uma determinada sala as reservas finalizadas antes deste horário.
cleanReservations :: LocalTime -> Room -> Room
cleanReservations timeNow room = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category=category room}
    where newSchedule = filter (\reservation -> finishTime reservation < timeNow) (schedule room)

-- | Esta função removerá de todas as salas as reservas cujo horário de final do evento já passou.
cleanAllReservations :: IO Bool
cleanAllReservations = do
    utcTimeNow <- getCurrentTime
    timeZone <- getTimeZone utcTimeNow
    let timeNow = utcToLocalTime timeZone utcTimeNow
    updateAllRooms (cleanReservations timeNow)        

-- | Dado um dia representado em uma tupla e um código de sala, esta função criará um relatório em texto com todas as reservas que esta sala tem para o dia especificado.
createReportForTheRoom :: (Integer, Int, Int) -> Room -> String
createReportForTheRoom (a, m, d) room = result
    where calendar = fromGregorian a m d
          reservations = filter (\reservation -> localDay (startTime reservation) == calendar) (schedule room)
          reservationsList = ("Relatório de ocupação para a sala " ++ code room ++ " no dia: " ++ show calendar ++ ":\n\n"): map show reservations
          result = intercalate "\n" reservationsList

-- | Dado um dia representado em uma tupla, esta função criará um relatório em texto para todas as reservas de todas as salas para o dia especificado.
createReportForTheDay :: (Integer, Int, Int) -> IO String
createReportForTheDay calendarTuple = do
    allRooms <- fetchRooms
    let reportsList = map (createReportForTheRoom calendarTuple) allRooms
        result = "Relatório de ocupação de salas:\n\n" ++ intercalate "\n\n===========\\===========\n\n" reportsList

    return result

-- | Com uma categoria especificada, esta função verificará o sistema e retornará a lista contendo todas as salas desta categoria.
searchRoomsCategory :: RoomCategory -> IO [Room]
searchRoomsCategory catRoom = do
    allRooms <- fetchRooms
    let filtered = filter (\room -> category room == catRoom) allRooms
    return filtered


-- | Com uma capacidade especificada, esta função verificará o sistema e retornará a lista contendo todas as salas com esta capacidade ou mais.
searchRoomsCapacity :: Int -> IO [Room]
searchRoomsCapacity capRoom = do
    allRooms <- fetchRooms
    let filtered = filter (\room -> capacity room >= capRoom) allRooms
    return filtered

-- | Com um horário de início e fim especificado em forma de tupla, esta função verificará o sistema e retornará a lista contendo todas as salas que estejam livres neste horário.
searchRoomsTime :: Maybe (Integer, Int, Int, Int, Int) -> Maybe (Integer, Int, Int, Int, Int) -> IO [Room]
searchRoomsTime startTimeTuple finishTimeTuple = do
    allRooms <- fetchRooms
    let startTime = makeTime startTimeTuple
        finishTime = makeTime finishTimeTuple
        filtered = filter (\room -> isFree room startTime && isFree room finishTime) allRooms
    return filtered

-- | Esta função recebe uma sala e um Resource e verifica se esta sala tem o recurso e se o tem na quantidade especificada ou superior, retornando a resposta em forma de valor booleano.
resourceIsEnough :: Room -> Resource -> Bool
resourceIsEnough room r | any (\roomResource -> (resourceKind roomResource == resourceKind r) && (resourceQuantity roomResource >= resourceQuantity r)) (resources room) = True
                        | otherwise = False

-- | Esta função verificará se uma sala contém todos os recursos em uma lista, e se os tem na quantidade especificada ou superior.
containsResources :: [Resource] -> Room -> Bool
containsResources [] _ = True
containsResources rs room = all (resourceIsEnough room) rs

-- | Com uma lista de recursos especificada, esta função verificará o sistema e retornará a lista contendo todas as salas que supram esta demanda.
searchRoomsResources :: Maybe [Resource] -> IO [Room]
searchRoomsResources resourcesNeeded = do
    allRooms <- fetchRooms
    let filtered = filter (containsResources resourcesNeeded) allRooms
    return filtered

-- | Dado um nome de usuário e uma sala, esta função verifica se a sala foi reservada por este usuário para algum horário e retorna a resposta em forma de valor booleano.
wasReservedBy :: String -> Room -> Bool
wasReservedBy requesterName room = result
    where result = any (\reservation -> requesterName == requester reservation) (schedule room)

-- | Com um nome de usuário especificado, esta função verificará o sistema e retornará a lista contendo todas as salas que foram reservadas para algum horário por aquele usuário.
searchRoomsRequester :: String -> IO [Room]
searchRoomsRequester requesterName = do
    allRooms <- fetchRooms
    let filtered = filter (\room -> wasReservedBy requesterName room) allRooms
    return filtered

-- | Esta função produzirá um texto contendo a lista de todas as categorias de sala suportadas pelo sistema.
printCategories :: IO ()
printCategories = do
    putStrLn "Qual categoria você deseja escolher?\n\
             \[L]aboratório\n\
             \[A]uditório\n\
             \[S]ala de aula\n\
             \[E]scritório\n\
             \[D]epósito"

-- | Esta função produzirá um texto listando todos os recursos oferecidos pelas salas do sistema.
printResources :: IO ()
printResources = do
    putStrLn "Qual recurso você deseja escolher?\n\
             \[P]rojetor\n\
             \[M]icroscópio\n\
             \[B]irô\n\
             \[C]omputador\n\
             \[Q]uadro\n\
             \[A]r condicionado"