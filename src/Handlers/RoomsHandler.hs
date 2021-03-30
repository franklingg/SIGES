module Handlers.RoomsHandler where

import Manager
import Handlers.DataHandler

instance Show Room where
    show(Room codeRoom _ _ capRoom localRoom catRoom) = "Dados da sala:                   \n\
                                                        \Código: "++ codeRoom ++         "\n\
                                                        \Capacidade: "++ show capRoom

instance Show Reservation where
    show(Reservation requester description time) = "Dados da reserva:                 \n\
                                                   \Data e horário: "++ show time ++ "\n\
                                                   \Responsável: "++ requester ++    "\n\
                                                   \Motivo: "++ description 

makeTime :: (Integer, Int, Int, Int) -> IO LocalTime
makeTime (a, m, d, h) = do
    let (Just clock) = makeTimeOfDayValid h 00 00
        calendar = fromGregorian a m d
        time = LocalTime calendar clock
    return time

isFree :: String -> LocalTime -> IO Bool
isFree codeRoom newTime = do
    (Just room) <- getRoom codeRoom
    let corresponding = filter (\reservation -> time reservation == newTime) (schedule room)
    return $ null corresponding

makeReservation :: String -> String -> String -> (Integer, Int, Int, Int) -> IO Bool
makeReservation codeRoom userName descriptionStr timeTuple = do
    timeReservation <- makeTime timeTuple
    free <- isFree codeRoom timeReservation
    if free then do
        (Just room) <- getRoom codeRoom
        let newReservation = Reservation{requester=userName, description=descriptionStr, time=timeReservation}
            newSchedule = newReservation:(schedule room)
            newRoom = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category =category room}
        operate <- deleteRoom codeRoom
        saveRoom newRoom
    else
        return False

deleteReservation :: String -> (Integer, Int, Int, Int) -> IO Bool
deleteReservation codeRoom timeTuple = do
    timeReservation <- makeTime timeTuple
    (Just room) <- getRoom codeRoom
    let newSchedule = filter (\reservation -> time reservation /= timeReservation) (schedule room)
        newRoom = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category=category room}
    operate <- deleteRoom codeRoom
    saveRoom newRoom

editReservation :: String -> (Integer, Int, Int, Int) -> (Integer, Int, Int, Int) -> IO Bool
editReservation codeRoom currentTimeTuple newTimeTuple = do
    currentTime <- makeTime currentTimeTuple
    newTime <- makeTime newTimeTuple
    (Just room) <- getRoom codeRoom
    let current = head $ filter (\reservation -> time reservation == newTime) (schedule room)
        newReservation = Reservation{requester=requester current, description=description current, time=newTime}
        deletedSchedule = filter (\reservation -> time reservation /= currentTime) (schedule room)
        newSchedule = newReservation:deletedSchedule
        newRoom = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category=category room}
    operate <- deleteRoom codeRoom
    saveRoom newRoom

cleanReservations :: LocalTime -> Room -> Room
cleanReservations timeNow room = Room{code=code room, schedule=newSchedule, resources=resources room, capacity=capacity room, localization=localization room, category=category room}
    where newSchedule = filter (\reservation -> time reservation > timeNow) (schedule room)

cleanAllReservations :: IO Bool
cleanAllReservations = do
    utcTimeNow <- getCurrentTime
    timeZone <- getTimeZone utcTimeNow
    (Just allRooms) <- fetchRooms
    let timeNow = utcToLocalTime timeZone utcTimeNow
        updated = map (cleanReservations timeNow) allRooms
    updateAllRooms updated        

