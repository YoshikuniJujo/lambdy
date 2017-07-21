module Main where

import Language.Lambdy.Parser

main :: IO ()
main = do
	print $ parseLambda "main = (putStrLn \"lambdy\")\n"
