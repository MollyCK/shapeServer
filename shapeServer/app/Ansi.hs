module Ansi where

data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq,Show,Enum)

-- cls empties a window
cls :: IO ()
cls = putStr "\ESC[2J"

-- goto can draw points at a particular region in a window
goto :: Int -> Int -> IO ()
goto x y    = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

color :: Colour -> String -> IO ()
color c s = putStr $ "\ESC[3" ++ show (fromEnum c) ++ "m" ++ s ++ "\ESC[0m"

