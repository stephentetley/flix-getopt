-- In GHCi: 
-- :set args -v --output=target.html -L/home/stephen

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

