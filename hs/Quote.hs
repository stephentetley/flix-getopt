-- In GHCi, :load Quote.hs, :set args and run main.

-- :set args --input=source_file1.txt --output=dest_file1.txt
-- Passes. 
-- > [Input "source_file1.txt",Output "dest_file1.txt"]

-- :set args --input=source file1.txt --output=dest file1.txt
-- Passes. The results make sense but not what we wanted:
-- > Opts: [Input "source",Output "dest"]
-- > Non-opts: ["file1.txt","file1.txt"]


-- :set args --input="source file1.txt" --output="dest file1.txt"
-- Passes. Quotes are retained in the options but they tokenizing by getOpt
-- > [Input "\"source file1.txt\"",Output "\"dest file1.txt\""]

-- :set args --input='source file1.txt' --output='dest file1.txt'
-- Passes. 
-- > Opts: [Input "'source",Output "'dest"]
-- > Non-opts: ["file1.txt'","file1.txt'"]

module Quote where

import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe )
import Text.Read ( readMaybe )

data Flag 
    = Input String
    | Output String
    deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['i']     ["input"]            (ReqArg Input "INFILE")      "source INFILE"
    , Option ['o']     ["output"]           (ReqArg Output "OUTFILE")    "target OUTFILE"
    ]


compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: <program> [OPTION...] files..."

main :: IO ()
main = do
    args <- getArgs
    mapM_ (\x -> putStrLn ("arg: " ++ x)) args
    (opts, nonopts) <- compilerOpts args
    putStrLn $ "Opts: " ++ show opts
    putStrLn $ "Non-opts: " ++ show nonopts
    return ()

