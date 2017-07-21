module Main where

import System.Environment
import Language.Lambdy.Parser

main :: IO ()
main = do
	fp : _ <- getArgs
	print . parseLambda =<< readFile fp
