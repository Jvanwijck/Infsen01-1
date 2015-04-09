module Parser where

import Data.Char as C
import Control.Applicative(Applicative(..))
import Control.Monad      (liftM, ap)

newtype Parser a = P (String -> [(a, String)])

instance Monad Parser where
  return v = P (\inp -> [(v,inp)])
  p >>= q  = P (\inp -> case parse p inp of
                             [] -> []
                             [(v,out)] -> parse (q v) out)

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap 

failure :: Parser a
failure = P (\inp -> [])

item :: Parser Char
item = P (\inp -> case inp of
                       [] -> []
                       (x:xs) -> [(x,xs)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp


(+++) :: Parser a -> Parser a -> Parser a
(+++) p q = P (\inp -> case parse p inp of
                          [] -> parse q inp
                          [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= (\x -> if p x then return x else failure)

many :: Parser a -> Parser [a]
many p = many' p +++ return []

many' :: Parser a -> Parser [a]
many' p = do
  v <- p
  vs <- many p
  return (v:vs)

nat :: Parser Int
nat = do
  xs <- many' digit
  return (read xs)

digit :: Parser Char
digit = sat C.isDigit

char :: Char -> Parser Char
char x = sat (x ==)

expr :: Parser Int
expr = do
  t <- term
  do
    char '+'
    e <- expr
    return (t + e)
   +++ return t

term :: Parser Int
term = do
  f <- factor
  do
    char '*'
    t <- term
    return (f * t)
   +++ return f

factor :: Parser Int
factor = do
  d <- nat
  return d
  +++ do
    char '('
    e <- expr
    char ')'
    return e

eval :: String -> Int
eval xs = fst(head(parse expr xs))