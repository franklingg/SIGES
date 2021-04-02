module Handlers.RoomsHandler where

import Manager
import Handlers.DataHandler
import Data.List

instance Show Room where
    show(Room codeRoom _ _ capRoom localRoom catRoom) = "Dados da sala:                   \n\
                                                        \Código: "++ codeRoom ++         "\n\
                                                        \Capacidade: "++ show capRoom

instance Show Reservation where
    show(Reservation requester description startTime finishTime) = "Dados da reserva:                 \n\
                                                                   \Início: "++ show startTime ++    "\n\
                                                                   \Fim: "++ show finishTime ++      "\n\
                                                                   \Responsável: " ++ requester ++   "\n\
                                                                   \Motivo: "++ description

instance Ord Reservation where
    compare res1 res2 = if startTime res1 <= startTime res2 then LT else GT

makeTime :: (Integer, Int, Int, Int, Int) -> LocalTime
makeTime (a, m, d, h, min) = time
    where (Just clock) = makeTimeOfDayValid h min 00
          calendar = fromGregorian a m d
          time = LocalTime calendar clock

isFree :: Room -> LocalTime -> Bool
isFree room newTime = not $ any (\reservation -> (startTime reservation <= newTime) && (finishTime reservation >= newTime)) (schedule room)

makeReservation :: String -> String -> String -> (Integer, Int, Int, Int, Int) -> (Integer, Int, Int, Int, Int) -> IO Bool
makeReservation codeRoom userName descriptionStr startTimeTuple finishTimeTuple = do
    let startTimeReservation = makeTime startTimeTuple
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

deleteReservation :: String -> String -> (Integer, Int, Int, Int, Int) -> IO Bool
deleteReservation codeRoom userName startTimeTuple = do
    let timeReservation = makeTime startTimeTuple
    (Just room) <- getRoom codeRoom
    
    if any (\reservation -> (startTime reservation == timeReservation) && (requester reservation == userName)) (schedule room)
        then do 
            let newSchedule = filter (\reservation -> startTime reservation /= timeReservation) (schedule room)
                newRoom = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category=category room}
            
            updateRoom codeRoom newRoom
            return True

        else return False

editReservation :: String -> String -> (Integer, Int, Int, Int, Int) -> (Integer, Int, Int, Int, Int) -> (Integer, Int, Int, Int, Int) -> IO Bool
editReservation codeRoom userName currentStartTimeTuple newStartTimeTuple newFinishTimeTuple = do
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

cleanReservations :: LocalTime -> Room -> Room
cleanReservations timeNow room = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category=category room}
    where newSchedule = filter (\reservation -> finishTime reservation > timeNow) (schedule room)

cleanAllReservations :: IO Bool
cleanAllReservations = do
    utcTimeNow <- getCurrentTime
    timeZone <- getTimeZone utcTimeNow
    let timeNow = utcToLocalTime timeZone utcTimeNow
    updateAllRooms (cleanReservations timeNow)        

createReportForTheRoom :: (Integer, Int, Int) -> Room -> String
createReportForTheRoom (a, m, d) room = result
    where calendar = fromGregorian a m d
          reservations = filter (\reservation -> localDay (startTime reservation) == calendar) (schedule room)
          reservationsList = ("Relatório de ocupação para a sala " ++ code room ++ " no dia: " ++ show calendar ++ ":\n\n"): map show reservations
          result = intercalate "\n" reservationsList

createReportForTheDay :: (Integer, Int, Int) -> IO String
createReportForTheDay calendarTuple = do
    allRooms <- fetchRooms
    let reportsList = map (createReportForTheRoom calendarTuple) allRooms
        result = "Relatório de ocupação de salas:\n\n" ++ intercalate "\n\n===========\\===========\n\n" reportsList

    return result

searchRoomsCategory :: Maybe RoomCategory -> IO [Room]
searchRoomsCategory catRoom = do
    allRooms <- fetchRooms
    if isJust catRoom  then do
        let filtered = filter (\room -> category room == fromJust catRoom) allRooms
        return filtered
    else do
        return allRooms

searchRoomsCapacity :: Maybe Int -> IO [Room]
searchRoomsCapacity capRoom = do
    allRooms <- fetchRooms
    if isJust capRoom then do
        let filtered = filter (\room -> capacity room >= fromJust capRoom) allRooms
        return filtered
    else do
        return allRooms

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

resourceIsEnough :: Room -> Resource -> Bool
resourceIsEnough room r | any (\roomResource -> (resourceKind roomResource == resourceKind r) && (resourceQuantity roomResource >= resourceQuantity r)) (resources room) = True
                        | otherwise = False

containsResources :: [Resource] -> Room -> Bool
containsResources [] _ = True
containsResources rs room = all (resourceIsEnough room) rs

searchRoomsResources :: Maybe [Resource] -> IO [Room]
searchRoomsResources resourcesNeeded = do
    allRooms <- fetchRooms
    if isJust resourcesNeeded then do
        let filtered = filter (containsResources (fromJust resourcesNeeded)) allRooms
        return filtered
    else do
        return allRooms

searchRoomsCombined :: Maybe RoomCategory -> Maybe Int -> Maybe (Integer, Int, Int, Int, Int) -> Maybe (Integer, Int, Int, Int, Int) -> Maybe [Resource] -> IO [Room]
searchRoomsCombined catRoom capRoom startTimeTuple finishTimeTuple resourcesNeeded = do
    filteredCategory <- searchRoomsCategory catRoom
    filteredCapacity <- searchRoomsCapacity capRoom
    filteredTime <- searchRoomsTime startTimeTuple finishTimeTuple
    filteredResources <- searchRoomsResources resourcesNeeded
    let filtered = filteredCategory `intersect` filteredCapacity `intersect` filteredTime `intersect` filteredResources
    return filtered
