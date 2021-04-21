{-# LANGUAGE OverloadedStrings #-}
import TUI.ScreenListeners (userInteraction)
import Manager (Screen (StartScreen), setTitle)
import System.Process

 {-
   Funcao para iniciar o sistema SIGES.
-}
loop :: Screen -> IO ()
loop currentScreen = do
    system "cls"
    nextScreen <- userInteraction currentScreen
    loop nextScreen

main = do
    setTitle "Sistema de Gerenciamento de Salas"
    loop StartScreen