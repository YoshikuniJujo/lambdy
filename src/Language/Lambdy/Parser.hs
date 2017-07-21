{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Language.Lambdy.Parser (parseLambda) where

import Data.Char
import Text.Papillon

data Definition
	= Var :=: Lambda
	deriving Show

data Lambda
	= Value Value
	| Lambda Var Lambda
	| Apply Lambda Lambda
	| Operator Op Lambda Lambda
	| Var Var
	deriving Show

data Value
	= Integer Integer
	| String String
	deriving Show

data Var
	= Name String
	deriving Show

data Op
	= Op String
	deriving Show

parseLambda :: String -> Either String [Definition]
parseLambda = either (Left . peMessage) (Right . fst)
	. runError . defs . parse

[papillon|

defs :: [Definition]
	= ds:def+			{ ds }

def :: Definition
	= v:var ' '* '=' ' '* l:lambda '\n'+
					{ v :=: l }

lambda :: Lambda
	= '(' f:lambda ' '* x:lambda ')'
					{ Apply f x }
	/ '(' x:lambda ' '* o:op ' '* y:lambda ')'
					{ Operator o x y }
	/ v:value			{ Value v }
	/ v:var				{ Var v }
	/ '\\' v:var ' '* '-' '>' ' '* l:lambda
					{ Lambda v l }

value :: Value
	= ds:<isDigit>+			{ Integer $ read ds }
	/ '"' s:<(/= '"')>* '"'		{ String s }

var :: Var
	= cs:<isAlpha>+			{ Name cs }

op :: Op
	= o:<isSymbol>+			{ Op o }

|]
