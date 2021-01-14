-- In GHCi, load Opt1.hs, set args and run main.

-- :set args -v --output=target.html -L/home/stephen
-- Passes.

-- :set args --ver --output=target.html -L/home/stephen
-- Fails. "--ver" is not a long enough prefix to identify a single option. 
-- Could be either "--verbose" or "--version"

-- :set args --vers --output=target.html -L/home/stephen
-- Passes. "--vers" is a long enough prefix to identify a single option "--version"

-- :set args -ver --output=target.html -L/home/stephen
-- Fails. "-ver" is recognized as three options 'v', 'e' and 'r' but 'e' and 'r' are not defined

module Opts1 where

import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe )

data Flag 
    = Verbose  | Version 
    | Input String | Output String | LibDir String
        deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
    , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
    , Option ['o']     ["output"]  (OptArg outp "FILE")  "output FILE"
    , Option ['c']     ["input"]   (OptArg inp  "FILE")  "input FILE"
    , Option ['L']     ["libdir"]  (ReqArg LibDir "DIR") "library directory"
    ]

inp,outp :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"
inp  = Input  . fromMaybe "stdin"

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
    print opts
    print nonopts
    return ()

