module Lexer where

import qualified Data.Text as T
import Data.Char (digitToInt)
import System.Random (randomRIO)
import Control.Monad.Trans (liftIO)

import Text.Parsec -- hiding (spaces)
--import Text.Parsec (letter)
import qualified Data.Map as M

type Str = T.Text
data SymbolVal = Val {value :: Constant, ret :: Maybe Int, endVal :: Maybe Int, step :: (Int -> Int)} | Nested (M.Map Int SymbolVal)
type SymbolTable = M.Map Char SymbolVal

newValue :: Constant -> SymbolVal
newValue v = Val v Nothing Nothing id
run = runParserT expr M.empty ""
s = T.pack "5/2 * 8"

type Parser = ParsecT Str SymbolTable IO

data Constant = IntV Int | StringV Str deriving (Show)
class Falsey a where
 bool :: a -> Bool

instance Falsey Int where
 bool 0 = False
 bool _ = True

instance Falsey T.Text where
 bool = not . T.null

instance Falsey Constant where
 bool (IntV v) = bool v
 bool (StringV v) = bool v

exprList :: Parser [Constant]
exprList = sepBy1 expr (symbol ",")
{-
spaces :: Parser ()
spaces = skipMany1 space
-}
symbol :: String -> Parser ()
symbol s = spaces >> string s >> spaces
stringP = string :: String -> Parser String

expr, andExpr, addExpr, multExpr, cmpExpr, notExpr, negateExpr, powerExpr, var, val, func, constant :: Parser Constant
expr = do
 e@(IntV v1) <- andExpr
 (do
  symbol "or"
  (IntV v2) <- expr
  return . IntV . fromEnum $ bool v1 || bool v2) <|> pure e

andExpr = do
 e@(IntV v1) <- notExpr
 (do
  symbol "and"
  (IntV v2) <- andExpr
  return . IntV . fromEnum $ bool v1 && bool v2) <|> pure e

notExpr = (do
 symbol "not"
 fmap (\(IntV v) -> IntV . fromEnum . not . bool $ v) cmpExpr) <|> cmpExpr

cmpExpr = do
 e@(IntV v1) <- addExpr
 spaces
 (do
   let go (s, f) = let p = try $ stringP s in (p >> pure f)
   f <- choice . map go $ [("=", (==)), ("<>", (/=)), (">", (>)), (">=", (>=)), ("<", (<)), ("<=", (<=))]
   spaces
   (IntV v2) <- cmpExpr
   return . IntV . fromEnum $ v1 `f` v2
  ) <|> pure e
addExpr = do
 e@(IntV v1) <- multExpr
 spaces
 (do
   let go (s, f) = let p = stringP s in (p >> pure f)
   f <- choice . map go  $ [("+", (+)), ("-", (-))]
   spaces
   (IntV v2) <- addExpr
   return . IntV $ v1 `f` v2
  ) <|> pure e
multExpr = do
 e@(IntV v1) <- negateExpr
 spaces
 (do
   let go (s, f) = let p = stringP s in (p >> pure f)
   f <- choice . map go  $ [("*", (*)), ("/", (quot))]
   spaces
   (IntV v2) <- multExpr
   return . IntV $ v1 `f` v2
  ) <|> pure e

negateExpr = (do
 string "-"
 fmap (\(IntV v) -> IntV $ negate v) powerExpr)
  <|> powerExpr

powerExpr = do
 spaces
 e@(IntV v1) <- val
 (do
  string "^"
  spaces
  (IntV v2) <- powerExpr
  pure $ IntV $ v1 ^ v2) <|> pure e

val = (symbol "(" *> expr <* symbol ")") <|> (var) <|> (func) <|> (constant)

arrayP :: Char -> Parser Constant
arrayP c = do
 symbol "("
 let 
  go [] (Val x _ _ _) = x
  go ((IntV v):es) (Nested m) = go es (m M.! v)
 es <- exprList
 m <- getState
 let res = go es (m M.! c)
 symbol ")"
 pure res




var = (do
 c <- letter
 (arrayP c) <|> (do
  m <- getState
  let (Val x _ _ _) = m M.! c
  pure x)
 )
func = (string "(") *> (int <|> rnd) <* (string ")") where
 int = string "int" >> expr
 rnd = do
  string "rnd"
  (IntV v) <- expr
  v' <- liftIO $ randomRIO (0, v)
  pure $ IntV v'

integer :: Parser Int
integer = go 0 where
 go :: Int -> Parser Int
 go acc = do
  c <- digit
  go' (10 * acc + digitToInt c)
 go' acc = go acc <|> pure acc

constant = (IntV <$> integer) <|> str
str = do
 char '"'
 s <- many alphaNum
 char '"'
 pure . StringV . T.pack $ s
{-
All of this together should make the process much easier
data Number = FloatV Double | IntV Int deriving (Show)
data Constant = StringV Str | NumV Number

type Id = Char
type IdList = [Id]

data Expr = BinFunc (Constant -> Constant -> Constant) Expr Expr | UnFunc (Constant -> Constant) Expr |
 Const Constant | Var Variable

data Variable = Variable Id [Expr]




evalExpr :: Expr -> Constant

exprP :: Parser Expr 

data Statement = Dim [Variable] | End | For Id Expr Expr (Maybe Expr) | Goto Int | Gosub Int | If Expr Int | 
 Input Str IdList | Let Variable Expr | Next IdList | On Expr [Int] | Print (Maybe Expr) [Expr] | 
 Rem | Ret




symbol :: String -> Parser ()
symbol s = spaces >> go s >> spaces where
 go [] = pure ()
 go (c : cs) = do
  _ <- char c
  go cs




instance Falsey T.Text where
 bool = not . T.null

spaces = skipMany1 space

applyBinOp :: (Int -> Int -> Int) -> Expr -> Expr -> Expr
applyBinOp f (IntV v1) (IntV v2) = IntV $ f v1 v2
applyBinOp _ _ _ = undefined

applyUnOp :: (Int -> Int) -> Expr -> Expr
applyUnOp f (IntV v) = IntV $ f v

expr :: Parser Expr
expr = do
 v1 <- andExpr
 (do 
  symbol "or"
  v2 <- expr
  pure $ BinOp ((applyBinOp (\x y -> fromEnum $ bool x || bool y)) v1 v2)) <|> pure v1

andExpr = do
 v1 <- notExpr
 (symbol "and" >> andExpr >>= \v2 -> pure $ BinOp ((applyBinOp (\x y -> fromEnum $ bool x && bool y)) v1 v2) ) <|> pure v1

notExpr = 
 (do
  symbol "not"
  v <- cmpExpr
  pure $ UnOp (applyUnOp negate) v) <|> cmpExpr

cmpExpr = undefined -}