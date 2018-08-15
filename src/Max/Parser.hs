module Parser
( Parser
, apply
, parse
, char
, spot
, token
, match
, star
, plus
, parseInt
, parseWord
) where

import MonadPlus
import Data.Char (isDigit)

-- Contains basic parser functionality necessary for the parsing of simple characters, numbers and strings, upon which the other types of parser will build (given the input will will consist entirely of text to be parsed).

-- Parser type definition
newtype Parser a = Parser (String -> [(a, String)])

-- Making Parser a functor
instance Functor Parser where
   fmap f (Parser p) = Parser (\s -> [(f a, b) | (a, b) <- p s])

-- Making Parser an applicative functor 
instance Applicative Parser where
   pure  = return
   (Parser p1) <*> (Parser p2) = Parser (\s -> [(f a, s2) | (f, s1) <- p1 s, (a, s2) <- p2 s1])

-- Making Parser an instance of monad
instance Monad Parser where
   return x = Parser (\s -> [(x,s)])
   m >>= k = Parser (\s ->
               [ (y, u) |
                 (x, t) <- apply m s,
                 (y, u) <- apply (k x) t ])

-- Making Parser an instance of monadplus
instance MonadPlus Parser where
   mzero = Parser (\s -> [])
   mplus m n = Parser (\s -> apply m s ++ apply n s)

-- Apply the parser (returns pair of parsed value and remaining string)
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) s = f s

-- Return parsed value, assuming at least one successful parse
parse :: Parser a -> String -> a
parse m s = one [ x | (x,t) <- apply m s, t == "" ]
  where
  one [] = error "no parse"
  one [x] = x
  one xs | length xs > 1 = error "ambiguous parse"

-- Create a parser to parse one character
char :: Parser Char
char = Parser f
  where
  f []     = []
  f (c:s)  = [(c,s)]

-- Parse (match?) a character statisfying a given condition
spot :: (Char -> Bool) -> Parser Char
spot p = do { c <- char; guard (p c); return c }

-- Create a parser to match a given character
token :: Char -> Parser Char
token c = spot (== c)

-- Create a parser to match a given string
--match :: String -> Parser String
--match []     = return []
--match (x:xs) = do {
--                 y <- token x;
--                 ys <- match xs;
--                 return (y:ys)
--               }
match :: String -> Parser String
match xs = sequence (map token xs)

-- Parsing sequences
-- Create a parser to match zero or more occurences (of a given parser match)
star :: Parser a -> Parser [a]
star p = plus p `mplus` return []

-- Create a parser to match one or more occurences (of a given parser match)
plus :: Parser a -> Parser [a]
plus p = do x <- p
            xs <- star p
            return (x:xs)

-- Parsing numbers
-- Create a parser to match a natural number 
parseNat :: Parser Int
parseNat = do s <- plus (spot isDigit)
              return (read s)

-- Create a parser to match a negative number
parseNeg :: Parser Int
parseNeg = do token '-'
              n <- parseNat
              return (-n)

-- Match an integer
parseInt :: Parser Int
parseInt = parseNat `mplus` parseNeg

-- Match a word
-- TODO maybe
parseWord :: Parser String
parseWord = do { s <- plus (spot (\s -> (s /= ' ') && (s /= ')') && (s /= ';')));
                 return s }