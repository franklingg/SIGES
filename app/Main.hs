{-# LANGUAGE OverloadedStrings #-}
import TUI.ScreenListeners (userInteraction)
import Manager (Screen (StartScreen))

loop :: Screen -> IO ()
loop currentScreen = do
    nextScreen <- userInteraction currentScreen
    loop nextScreen
    return ()

main = loop StartScreen