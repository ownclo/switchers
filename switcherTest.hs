{-# LANGUAGE TypeOperators #-}

import Switcher

data SIGN = PLUS | NONE | MINUS -- for lulz only
          deriving (Enum, Bounded)

composedSwitcher :: Switcher (Int :& Bool :& SIGN)
composedSwitcher = rangeSwitcher (0,3) <|< enumSwitcher <|< enumSwitcher

-- still can be checked by hand.
composedRange :: [Int]
composedRange =
    [ csw (-1 :& False :& PLUS)  -- 0
    , csw (-1 :& True  :& MINUS) -- 5
    , csw (0  :& False :& PLUS)  -- 0
    , csw (0  :& True  :& NONE)  -- 4
    , csw (1  :& False :& PLUS)  -- 6
    , csw (1  :& True  :& PLUS)  -- 9
    , csw (2  :& False :& PLUS)  -- 12
    , csw (2  :& True  :& NONE)  -- 16
    , csw (3  :& False :& MINUS) -- 20
    , csw (3  :& True  :& PLUS)  -- 21
    , csw (4  :& False :& PLUS)  -- 18
    , csw (4  :& True  :& PLUS)  -- 21
    ] where csw = switch composedSwitcher

composedTest :: Bool
composedTest = composedRange ==
    [0, 5, 0, 4, 6, 9, 12, 16, 20, 21, 18, 21]

printPrefix :: Show a => String -> a -> IO ()
printPrefix s v = putStrLn $ s ++ show v

-- can I create an alias for tuple construction operator?
main :: IO ()
main = printPrefix "Range: " (range composedSwitcher) -- 4*2*3 = 24
    >> printPrefix "Test passed: " composedTest
    >> mapM_ print composedRange
