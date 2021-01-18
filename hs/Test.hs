-- In GHCi, :load Test.hs, :set args and run main.

-- :set args -XcBlue
-- Passes. [HiRes, Colour(Just("Blue"))]

-- :set args -XcBluew1200
-- Passes. [HiRes, Colour(Just("Bluew1200"))]
-- Warning - It is not reall pappropriate to coalesce short options that take arguments

-- :set args -XcBlue -w1200

module Test where

import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe )
import Text.Read ( readMaybe )

data Flag 
    = HiRes
    | Colour (Maybe String)
    | Width Int 
    | Height Int
    deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['X']     ["hires"]            (NoArg HiRes)             "show in high resolution"
    , Option ['C','c'] ["colour", "color"]  (OptArg Colour "COLOUR")    "add filter of COLOUR"
    , Option ['w']     ["width"]            (ReqArg width "WIDTH")      "image WIDTH"
    , Option ['h']     ["height"]           (ReqArg height "HEIGHT")    "image HEIGHT"
    ]

width :: String -> Flag
width s = case readMaybe s of 
    Just i -> Width i
    Nothing -> Width 1000


height :: String -> Flag
height s = case readMaybe s of 
    Just i -> Height i
    Nothing -> Height 1000



compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ic [OPTION...] files..."

main :: IO ()
main = do
    args <- getArgs
    mapM_ (\x -> putStrLn ("arg: " ++ x)) args
    (opts, nonopts) <- compilerOpts args
    putStrLn $ "Opts: " ++ show opts
    putStrLn $ "Non-opts: " ++ show nonopts
    return ()

