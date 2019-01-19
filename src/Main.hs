{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Foreign.C.Types (CInt)
import Linear (V4(..))
import SDL

width :: CInt
width = 640

height :: CInt
height = 480

main :: IO ()
main = do
    initializeAll
    window <- createWindow
                "Hello World"
                defaultWindow { windowInitialSize = V2 width height }
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer
    destroyWindow window
    putStrLn "Done"

appLoop :: Renderer -> IO ()
appLoop renderer = do
    events <- pollEvents
    let eventIsQPress event =
            case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                        keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                _ -> False
        qPressed = any eventIsQPress events

    rendererDrawColor renderer $= V4 0 0 255 255
    clear renderer

    rendererDrawColor renderer $= V4 255 0 0 255
    drawLine renderer (P (V2 0 0)) (P (V2 (width - 1) (height - 1)))

    present renderer

    unless qPressed (appLoop renderer)
