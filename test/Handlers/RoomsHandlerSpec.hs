module Handlers.RoomsHandlerSpec where

import SpecHelper

createNewRoom :: Room
createNewRoom = Room{code="Sala", schedule=[], resources=[], capacity=100, localization="Rua dos bobos", category = Classroom}

salaZero :: Room
salaZero = Room{code="SalaZero", schedule=[Reservation{requester="Patati",description="Show",startTime=LocalTime{localDay=fromGregorian 2015 12 17,localTimeOfDay=fromJust $ makeTimeOfDayValid 8 0 0},finishTime=LocalTime{localDay=fromGregorian 2015 12 17,localTimeOfDay=fromJust $ makeTimeOfDayValid 10 0 0}}, Reservation{requester="Patati",description="Show",startTime=LocalTime{localDay=fromGregorian 2015 12 17,localTimeOfDay=fromJust $ makeTimeOfDayValid 12 0 0},finishTime=LocalTime{localDay=fromGregorian 2015 12 17,localTimeOfDay=fromJust $ makeTimeOfDayValid 14 0 0}}], resources=[], capacity=100, localization="Bloco BL", category = Classroom}

salaUm :: Room
salaUm = Room{code="SalaUm", schedule=[], resources=[Resource{resourceKind = Microscope, resourceQuantity = 5}, Resource{resourceKind = Projector, resourceQuantity = 1},Resource{resourceKind = Computer, resourceQuantity = 1}], capacity=1, localization="Bloco BL", category = Laboratory}

spec :: Spec
spec = do 
    
    describe "makeReservation" $ do
        it "Ao criar uma reserva nova" $
            makeReservation "SalaZero" "Dida Ritz" "Performing class" (1995, 12, 17, 4, 0) (1995, 12, 17, 6, 30) `shouldReturn` True
        it "Ao criar uma reserva nova" $
            makeReservation "SalaZero" "Patati" "Show" (2005, 12, 17, 4, 0) (2005, 12, 17, 6, 30) `shouldReturn` True
        it "Ao criar uma reserva nova" $
            makeReservation "SalaZero" "Patati" "Show" (2015, 12, 17, 8, 0) (2015, 12, 17, 10, 0) `shouldReturn` True
        it "Ao criar uma reserva num horário ocupado" $
            makeReservation "SalaZero" "Dida Ritz" "Performing class" (1995, 12, 17, 4, 0) (1995, 12, 17, 6, 30) `shouldReturn` False
        it "Ao criar uma reserva em um horário parcialmente ocupado" $
            makeReservation "SalaZero" "Dida Ritz" "Performing class" (1995, 12, 17, 3, 0) (1995, 12, 17, 4, 30) `shouldReturn` False

    describe "deleteReservation" $ do
        it "Ao se deletar uma reserva válida mas com um userName errado" $
            deleteReservation "SalaZero" "Wrong User" (1995, 12, 17, 4, 0) `shouldReturn` False 
        it "Ao se deletar uma reserva válida" $
            deleteReservation "SalaZero" "Dida Ritz" (1995, 12, 17, 4, 0) `shouldReturn` True 
        it "Ao se deletar uma reserva que não existe" $
            deleteReservation "SalaZero" "Dida Ritz" (1995, 12, 17, 4, 0) `shouldReturn` False 
    
    describe "editReservation" $ do
        it "Ao se editar uma reserva que existe mas com um userName errado" $
            editReservation "SalaZero" "Wrong User" (2005, 12, 17, 4, 0) (2015, 12, 17, 10, 0) (2015, 12, 17, 12, 0) `shouldReturn` False
        it "Ao se editar uma reserva que existe com dados corretos, mas com um novo horário já ocupado" $
            editReservation "SalaZero" "Patati" (2005, 12, 17, 4, 0) (2015, 12, 17, 8, 0) (2015, 12, 17, 10, 0) `shouldReturn` False
        it "Ao se editar uma reserva que existe com dados corretos, mas com um novo horário parcialmente ocupado" $
            editReservation "SalaZero" "Patati" (2005, 12, 17, 4, 0) (2015, 12, 17, 7, 0) (2015, 12, 17, 9, 0) `shouldReturn` False
        it "Ao se editar uma reserva que existe com dados corretos" $
            editReservation "SalaZero" "Patati" (2005, 12, 17, 4, 0) (2015, 12, 17, 12, 0) (2015, 12, 17, 14, 0) `shouldReturn` True
        it "Ao se editar uma reserva que não existe" $
            editReservation "SalaZero" "Dida Ritz" (1995, 12, 17, 4, 0) (2015, 12, 17, 10, 0) (2015, 12, 17, 12, 0) `shouldReturn` False

    describe "searchRoomsCategory" $ do
        it "Ao se buscar com a categoria adequada" $
            searchRoomsCategory (Just Classroom) `shouldReturn` [salaZero]
        it "Ao se buscar com Nothing" $
            searchRoomsCategory Nothing `shouldReturn` [salaUm, salaZero]
        it "Ao se buscar com Categoria sem salas" $
            searchRoomsCategory (Just Auditorium) `shouldReturn` []

    describe "searchRoomsCapacity" $ do
        it "Ao se buscar capacidade além da que qualquer sala pode oferecer" $
            searchRoomsCapacity (Just 1000) `shouldReturn` []
        it "Ao se buscar capacidade abaixo da oferecida por todas as salas" $
            searchRoomsCapacity (Just 0) `shouldReturn` [salaUm, salaZero]
        it "Ao se buscar com Nothing" $
            searchRoomsCapacity Nothing `shouldReturn` [salaUm, salaZero]
        it "Ao se buscar com uma capacidade qualquer" $
            searchRoomsCapacity (Just 80) `shouldReturn` [salaZero]

    describe "searchRoomsTime" $ do
        it "Ao se procurar num horário vago para todas as salas" $
            searchRoomsTime (Just (2017, 04, 04, 12, 0)) (Just (2017, 04, 04, 14, 0)) `shouldReturn` [salaUm, salaZero]
        it "Ao se procurar num horário qualquer" $
            searchRoomsTime (Just (2015, 12, 17, 10, 0)) (Just (2015, 12, 17, 12, 0)) `shouldReturn` [salaUm]
        it "Ao se procurar com horarios igual a Nothing" $
            searchRoomsTime Nothing Nothing `shouldReturn` [salaUm, salaZero]

    describe "searchRoomsResources" $ do
        it "Ao se usar Nothing na busca" $
            searchRoomsResources Nothing `shouldReturn` [salaUm, salaZero]
        it "Ao se buscar um recurso que nenhuma sala possui" $
            searchRoomsResources (Just [Resource{resourceKind = Desk, resourceQuantity = 1}]) `shouldReturn` []
        it "Ao se buscar um recurso em quantidade maior que a oferecida por qualquer sala" $
            searchRoomsResources (Just [Resource{resourceKind = Microscope, resourceQuantity = 6}]) `shouldReturn` []
        it "Ao se buscar um recurso não oferecido com recursos oferecidos" $
            searchRoomsResources (Just [Resource{resourceKind = Microscope, resourceQuantity = 5}, Resource{resourceKind = Projector, resourceQuantity = 1},Resource{resourceKind = Computer, resourceQuantity = 1}, Resource{resourceKind = Desk, resourceQuantity = 1}]) `shouldReturn` []
        it "Ao se buscar com uma lista de recursos" $
            searchRoomsResources (Just [Resource{resourceKind = Microscope, resourceQuantity = 5}, Resource{resourceKind = Projector, resourceQuantity = 1},Resource{resourceKind = Computer, resourceQuantity = 1}]) `shouldReturn` [salaUm]

    describe "searchRoomsCombined" $ do
        it "Ao se colocar Nothing em todos os parâmetros" $
            searchRoomsCombined Nothing Nothing Nothing Nothing Nothing `shouldReturn` [salaUm, salaZero]
        it "Ao se colocar Nothing na categoria" $
            searchRoomsCombined Nothing (Just 50) (Just (2021, 04, 04, 18, 45)) (Just (2021, 04, 04, 20, 15)) (Just []) `shouldReturn` [salaZero]
        it "Ao se colocar Nothing na capacidade" $
            searchRoomsCombined (Just Classroom) Nothing (Just (2021, 04, 04, 18, 45)) (Just (2021, 04, 04, 20, 15)) (Just []) `shouldReturn` [salaZero]
        it "Ao se colocar Nothing no horário" $
            searchRoomsCombined (Just Classroom) (Just 50) Nothing Nothing (Just []) `shouldReturn` [salaZero]
        it "Ao se colocar Nothing na lista de recursos" $
            searchRoomsCombined (Just Classroom) (Just 50) (Just (2021, 04, 04, 18, 45)) (Just (2021, 04, 04, 20, 15)) Nothing `shouldReturn` [salaZero]
        it "Ao se buscar com todos os parâmetros sem salas que satisfaçam a busca" $
            searchRoomsCombined (Just Classroom) (Just 50) (Just (2021, 04, 04, 18, 45)) (Just (2021, 04, 04, 20, 15)) (Just [Resource{resourceKind = Microscope, resourceQuantity = 5}, Resource{resourceKind = Projector, resourceQuantity = 1},Resource{resourceKind = Computer, resourceQuantity = 1}]) `shouldReturn` []
        it "Ao se buscar com todos os parâmetros" $
            searchRoomsCombined (Just Laboratory) (Just 1) (Just (2021, 04, 04, 18, 45)) (Just (2021, 04, 04, 20, 15)) (Just [Resource{resourceKind = Microscope, resourceQuantity = 5}, Resource{resourceKind = Projector, resourceQuantity = 1},Resource{resourceKind = Computer, resourceQuantity = 1}]) `shouldReturn` [salaUm]

    describe "cleanAllReservations" $ do
        it "Ao limpar as reservas" $
            cleanAllReservations `shouldReturn` True        

main = hspec spec