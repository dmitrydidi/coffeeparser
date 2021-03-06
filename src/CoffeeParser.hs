module CoffeeParser
    ( useParser, extractString, extract) where
import Data.Char
import Control.Applicative

newtype Parser a = P( String -> [(a, String )] )

useParser :: Parser a -> String -> [(a, String)]
useParser (P p) input = p input

extract :: Parser Char
extract = P $ (\input -> case input of
                    [] -> []
                    (x:xs) -> [(x,xs)])

instance Functor Parser where
    fmap f p = P $ \input -> case useParser p input of
                    [] -> []
                    [(v, rest)] -> [(f v, rest)]

instance Applicative Parser where
    pure x = P $ \input -> [(x, input)]
    -- <*> :: Parser(a -> b) -> Parser a -> Parser b
    pf <*> pg = P $ \input -> case useParser pf input of
                        [] -> []
                        [(f, rest)] -> useParser (fmap f pg) rest

instance Monad Parser where
    -- >>= :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P $ \input -> case useParser p input of
                      [] -> []
                      [(p', rest)] -> useParser (f p') rest

instance Alternative Parser where
    empty = P $ \ input -> []
    l <|> r = P $ \input -> case useParser l input of
                            [] -> useParser r input
                            [(x,rest)] -> [(x,rest)]

satisfy:: (Char -> Bool) -> Parser Char
satisfy predicate = do
                       x <- extract
                       if predicate x
                           then return x
                           else empty
char:: Char -> Parser Char
char c = satisfy (==c)

extractString:: String -> Parser String
extractString [] = return []
extractString (x:xs) = char x >>= \ x' -> extractString xs >>= \ xs' -> return (x':xs')

digit:: Parser Char
digit = satisfy isDigit

lower:: Parser Char
lower = satisfy isLower

upper:: Parser Char
upper = satisfy isUpper

alphanum:: Parser Char
alphanum = satisfy isAlphaNum

ident:: Parser String
ident = lower >>= \ x -> many alphanum >>= \ xs -> return (x:xs)

nat:: Parser Int
nat = some digit >>= \ xs -> return (read xs)

space:: Parser ()
space = many (satisfy isSpace) >> return ()

int:: Parser Int
int = (char '-' >>= \ ns -> nat >>= \n -> return (-n)) <|> nat

token::Parser a -> Parser a
token parse = space >>= \ out -> parse >>= \out' -> space >> return (out')

natural:: Parser Int
natural = token nat

integer:: Parser Int
integer = token int

symbol:: String -> Parser String
symbol xs = token $ extractString xs

expr :: Parser Int
expr = term >>= \t -> (symbol "+" >>= \t' -> expr >>= \e -> return (t+e))
                <|> (symbol "-" >>= \s' -> expr >>= \e' -> return (t-e'))
                <|> return t

term :: Parser Int
term = do
         f <- exponentiation
         do
             symbol "*"
             t <-term
             return (f*t)
             <|> do
                    symbol "/"
                    t' <- term
                    return (f `div` t')
             <|> return f

exponentiation:: Parser Int
exponentiation = do
                   f <- factor
                   do
                       symbol "^"
                       f' <- exponentiation
                       return (f^f')
                       <|> return f
factor :: Parser Int
factor = (symbol "(" >>=
           \t -> expr >>=
           \e -> symbol ")" >>
           return e )
          <|> integer

eval :: String -> Int
eval xs = case  (useParser expr xs) of
              [(n, [])] -> n
              [(_,out)] -> error ("Unused input" ++ out)
              [] -> error "Invalid input"