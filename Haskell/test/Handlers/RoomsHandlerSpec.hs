module Handlers.RoomsHandlerSpec where

import SpecHelper

salaZero :: Room
salaZero = Room{code="SALAZERO", schedule=[Reservation{requester="Patati",description="Show",startTime=LocalTime{localDay=fromGregorian 2015 12 17,localTimeOfDay=fromJust $ makeTimeOfDayValid 8 0 0},finishTime=LocalTime{localDay=fromGregorian 2015 12 17,localTimeOfDay=fromJust $ makeTimeOfDayValid 10 0 0}}, Reservation{requester="Patati",description="Show",startTime=LocalTime{localDay=fromGregorian 2015 12 17,localTimeOfDay=fromJust $ makeTimeOfDayValid 12 0 0},finishTime=LocalTime{localDay=fromGregorian 2015 12 17,localTimeOfDay=fromJust $ makeTimeOfDayValid 14 0 0}}], resources=[], capacity=100, localization="Bloco BL", category = Classroom}

salaUm :: Room
salaUm = Room{code="SALAUM", schedule=[], resources=[Resource{resourceKind = Microscope, resourceQuantity = 5}, Resource{resourceKind = Projector, resourceQuantity = 1},Resource{resourceKind = Computer, resourceQuantity = 1}], capacity=1, localization="Bloco BL", category = Laboratory}

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
            searchRoomsCategory Classroom `shouldReturn` [salaZero]
        it "Ao se buscar com Categoria sem salas" $
            searchRoomsCategory Auditorium `shouldReturn` []

    describe "searchRoomsCapacity" $ do
        it "Ao se buscar capacidade além da que qualquer sala pode oferecer" $
            searchRoomsCapacity 1000 `shouldReturn` []
        it "Ao se buscar capacidade abaixo da oferecida por todas as salas" $
            searchRoomsCapacity 0 `shouldReturn` [salaUm, salaZero]
        it "Ao se buscar com uma capacidade qualquer" $
            searchRoomsCapacity 80 `shouldReturn` [salaZero]

    describe "searchRoomsTime" $ do
        it "Ao se procurar num horário vago para todas as salas" $
            searchRoomsTime (2017, 04, 04, 12, 0) (2017, 04, 04, 14, 0) `shouldReturn` [salaUm, salaZero]
        it "Ao se procurar num horário qualquer" $
            searchRoomsTime (2015, 12, 17, 10, 0) (2015, 12, 17, 12, 0) `shouldReturn` [salaUm]

    describe "searchRoomsResources" $ do
        it "Ao se buscar um recurso que nenhuma sala possui" $
            searchRoomsResources [Resource{resourceKind = Desk, resourceQuantity = 1}] `shouldReturn` []
        it "Ao se buscar um recurso em quantidade maior que a oferecida por qualquer sala" $
            searchRoomsResources [Resource{resourceKind = Microscope, resourceQuantity = 6}] `shouldReturn` []
        it "Ao se buscar um recurso não oferecido com recursos oferecidos" $
            searchRoomsResources [Resource{resourceKind = Microscope, resourceQuantity = 5}, Resource{resourceKind = Projector, resourceQuantity = 1},Resource{resourceKind = Computer, resourceQuantity = 1}, Resource{resourceKind = Desk, resourceQuantity = 1}] `shouldReturn` []
        it "Ao se buscar com uma lista de recursos" $
            searchRoomsResources [Resource{resourceKind = Microscope, resourceQuantity = 5}, Resource{resourceKind = Projector, resourceQuantity = 1},Resource{resourceKind = Computer, resourceQuantity = 1}] `shouldReturn` [salaUm]

    describe "cleanAllReservations" $ do
        it "Ao limpar as reservas" $
            cleanAllReservations `shouldReturn` True

main = hspec spec