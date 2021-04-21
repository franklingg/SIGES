module Handlers.RoomsHandler where

-- importa a entidade Manager
import Manager
-- importa a entidade DataHandler
import Handlers.DataHandler
-- importa List
import Data.List

-- Room e uma instancia da classe de tipo Show
instance Show Room where
    show(Room codeRoom _ _ capRoom localRoom catRoom) = "Dados da sala:                   \n\
                                                        \Código: "++ codeRoom ++         "\n\
                                                        \Capacidade: "++ show capRoom

-- Reservation e uma instancia da classe de tipo Show
instance Show Reservation where
    show(Reservation requester description startTime finishTime) = "Dados da reserva:                 \n\
                                                                   \Início: "++ show startTime ++    "\n\
                                                                   \Fim: "++ show finishTime ++      "\n\
                                                                   \Responsável: " ++ requester ++   "\n\
                                                                   \Motivo: "++ description

-- Reservation e uma instancia da classe de tipo Ord
instance Ord Reservation where
    compare res1 res2 = if startTime res1 <= startTime res2 then LT else GT

{-
   Funcao para arranjar o tempo.
-}
makeTime :: (Integer, Int, Int, Int, Int) -> LocalTime
makeTime (a, m, d, h, min) = time
    where (Just clock) = makeTimeOfDayValid h min 00
          calendar = fromGregorian a m d
          time = LocalTime calendar clock

{-
   Funcao para verificar se esta livre.
-}
isFree :: Room -> LocalTime -> Bool
isFree room newTime = not $ any (\reservation -> (startTime reservation <= newTime) && (finishTime reservation >= newTime)) (schedule room)

{-
   Funcao para realizar uma reserva.
-}
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

{-
   Funcao para deletar uma reserva.
-}
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

findReservation :: String -> (Integer,Int,Int,Int,Int)-> String -> IO Reservation
findReservation codeRoom startTimeTuple userName = do
    (Just room) <- getRoom codeRoom
    let reservations = schedule room
        matching = filter (\reservation -> (startTime reservation == makeTime startTimeTuple) && (requester reservation == userName)) reservations
    return $ head matching


{-
   Funcao para editar uma reserva.
-}
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

{-
   Funcao para retirar as reservas que ja passaram do tempo.
-}
cleanReservations :: LocalTime -> Room -> Room
cleanReservations timeNow room = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category=category room}
    where newSchedule = filter (\reservation -> finishTime reservation < timeNow) (schedule room)

{-
   Funcao para retirar todas as reservas que ja passaram do tempo.
-}
cleanAllReservations :: IO Bool
cleanAllReservations = do
    utcTimeNow <- getCurrentTime
    timeZone <- getTimeZone utcTimeNow
    let timeNow = utcToLocalTime timeZone utcTimeNow
    updateAllRooms (cleanReservations timeNow)        

{-
   Funcao para criar um relatorio para a sala.
-}
createReportForTheRoom :: (Integer, Int, Int) -> Room -> String
createReportForTheRoom (a, m, d) room = result
    where calendar = fromGregorian a m d
          reservations = filter (\reservation -> localDay (startTime reservation) == calendar) (schedule room)
          reservationsList = ("Relatório de ocupação para a sala " ++ code room ++ " no dia: " ++ show calendar ++ ":\n\n"): map show reservations
          result = intercalate "\n" reservationsList

{-
   Funcao para criar um relatorio para o dia.
-}
createReportForTheDay :: (Integer, Int, Int) -> IO String
createReportForTheDay calendarTuple = do
    allRooms <- fetchRooms
    let reportsList = map (createReportForTheRoom calendarTuple) allRooms
        result = "Relatório de ocupação de salas:\n\n" ++ intercalate "\n\n===========\\===========\n\n" reportsList

    return result

{-
   Funcao para procurar sala por categoria.
-}
searchRoomsCategory :: Maybe RoomCategory -> IO [Room]
searchRoomsCategory catRoom = do
    allRooms <- fetchRooms
    if isJust catRoom  then do
        let filtered = filter (\room -> category room == fromJust catRoom) allRooms
        return filtered
    else do
        return allRooms

{-
   Funcao para procurar sala por capacidade.
-}
searchRoomsCapacity :: Maybe Int -> IO [Room]
searchRoomsCapacity capRoom = do
    allRooms <- fetchRooms
    if isJust capRoom then do
        let filtered = filter (\room -> capacity room >= fromJust capRoom) allRooms
        return filtered
    else do
        return allRooms

{-
   Funcao para procurar sala por tempo.
-}
searchRoomsTime :: Maybe (Integer, Int, Int, Int, Int) -> Maybe (Integer, Int, Int, Int, Int) -> IO [Room]
searchRoomsTime startTimeTuple finishTimeTuple = do
    allRooms <- fetchRooms
    if isJust startTimeTuple && isJust finishTimeTuple then do
        let startTime = makeTime (fromJust startTimeTuple)
            finishTime = makeTime (fromJust finishTimeTuple)
            filtered = filter (\room -> isFree room startTime && isFree room finishTime) allRooms
        return filtered
    else do
        return allRooms

{-
   Funcao para verificar se o recurso e suficiente.
-}
resourceIsEnough :: Room -> Resource -> Bool
resourceIsEnough room r | any (\roomResource -> (resourceKind roomResource == resourceKind r) && (resourceQuantity roomResource >= resourceQuantity r)) (resources room) = True
                        | otherwise = False

{-
   Funcao para verificar se contem recurso.
-}
containsResources :: [Resource] -> Room -> Bool
containsResources [] _ = True
containsResources rs room = all (resourceIsEnough room) rs

{-
   Funcao para pesquisar recursos das salas.
-}
searchRoomsResources :: Maybe [Resource] -> IO [Room]
searchRoomsResources resourcesNeeded = do
    allRooms <- fetchRooms
    if isJust resourcesNeeded then do
        let filtered = filter (containsResources (fromJust resourcesNeeded)) allRooms
        return filtered
    else do
        return allRooms

{-
   Funcao para pesquisar salas com recursos combinados.
-}
searchRoomsCombined :: Maybe RoomCategory -> Maybe Int -> Maybe (Integer, Int, Int, Int, Int) -> Maybe (Integer, Int, Int, Int, Int) -> Maybe [Resource] -> IO [Room]
searchRoomsCombined catRoom capRoom startTimeTuple finishTimeTuple resourcesNeeded = do
    filteredCategory <- searchRoomsCategory catRoom
    filteredCapacity <- searchRoomsCapacity capRoom
    filteredTime <- searchRoomsTime startTimeTuple finishTimeTuple
    filteredResources <- searchRoomsResources resourcesNeeded
    let filtered = filteredCategory `intersect` filteredCapacity `intersect` filteredTime `intersect` filteredResources
    return filtered
