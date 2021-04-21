{-# LANGUAGE OverloadedStrings #-}
-- importa a entidade ScreenListeners
import TUI.ScreenListeners (userInteraction)
-- importa a entidade Manager
import Manager (Screen (StartScreen))

 {-
   Funcao para iniciar o sistema SIGES.
-}
loop :: Screen -> IO ()
loop currentScreen = do
    nextScreen <- userInteraction currentScreen
    loop nextScreen
    return ()

main = loop StartScreen