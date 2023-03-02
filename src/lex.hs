module Lex where




import Text.Parsec
import qualified Data.Text as T
import Data.Char (digitToInt, isPrint)


run (Left err) = show err; run (Right v) = show v
go p = parse p "" . T.toLower . T.pack
test p = putStrLn . run . go p



type Id = Char
type IdList = [Id]

data Statement = Dim [Variable] | End | For Id Expr Expr (Maybe Expr) | Goto Int | Gosub Int | If Expr Int | 
 Input Str IdList | Let (Variable) Expr | Next IdList | On Expr [Int] | Print (Maybe Expr) [Expr] | Rem | Ret
 deriving Show

type Str = T.Text

type Parser = Parsec Str ()

-- Parse a single string
str :: String -> Parser ()
str = foldr go (pure ()) where
 go :: Char -> Parser () -> Parser ()
 go c acc = char c >> acc

data Number = FloatV Double | IntV Int deriving (Eq, Ord)
data Constant = StringV Str | NumV Number deriving (Eq, Ord)

instance Show Number where
 show (FloatV v) = show v
 show (IntV v) = show v

instance Show Constant where
 show (StringV v) = show v
 show (NumV v) = show v

applyPow :: ((Int -> Int -> Int), (Double -> Double -> Double), (Double -> Int -> Double)) -> Constant -> Constant -> Constant
applyPow _ (StringV _) _ = undefined
applyPow _ _ (StringV _) = undefined
applyPow (f1,f2,f3) (NumV x) (NumV y) = NumV $ go x y where
 go (IntV v1) (IntV v2) = IntV $ v1 `f1` v2
 go (FloatV v1) (FloatV v2) = FloatV $ v1 `f2` v2
 go (FloatV v1) (IntV v2) = FloatV $ v1 `f3` v2
 go (IntV v1) (FloatV v2) = FloatV $ (fromIntegral v1) `f2` v2

--          (+)                   (+)                           (-)                     (-)
apply :: ((Int -> Int -> Int), (Double -> Double -> Double), (Int -> Int -> Int), (Double -> Double -> Double)) 
 -> Bool -> Constant -> Constant -> Constant
apply _ _ (StringV _) _ = undefined
apply _ _ _ (StringV _) = undefined
apply (f1, f2, f3, f4) c (NumV x) (NumV y) = NumV $ go x y where
 go (FloatV x) (FloatV y) = let f = if c then f2 else f4 in FloatV $ f x y
 go (IntV x) (IntV y) = let f = if c then f1 else f3 in IntV $ f x y
 go (FloatV x) (IntV y) = let f = if c then f2 else f4 in FloatV $ f x (fromIntegral y)
 go (IntV x) (FloatV y) = let f = if c then f2 else f4 in FloatV $ f (fromIntegral x) y

--               (*)                         (*)                     div                       (/)
applyMult :: ((Int -> Int -> Int), (Double -> Double -> Double), (Int -> Int -> Int), (Double -> Double -> Double))
 -> Bool -> Constant -> Constant -> Constant
applyMult _ _ (StringV _) _ = undefined
applyMult _ _ _ (StringV _) = undefined
applyMult (f1,f2,f3,f4) c (NumV x) (NumV y) = NumV $ go x y where
 go (IntV x) (IntV y) = let f = if c then f1 else f3 in IntV $ f x y
 go (FloatV x) (FloatV y) = let f = if c then f2 else f4 in FloatV $ f x y
 go (FloatV x) (IntV y) = let f = if c then f2 else f4 in FloatV $ f x (fromIntegral y)
 go (IntV x) (FloatV y) = let f = if c then f2 else f4 in FloatV $ f (fromIntegral x) y



data Expr = BinFunc (Constant -> Constant -> Constant) Expr Expr | UnFunc (Constant -> Constant) Expr |
 Const Constant | Var Variable | Rnd Expr


data Variable = Variable Id [Expr] deriving Show

tol = 1e-6 :: Double


-- isClose :: Fractional a => a -> a -> Bool
isClose x y = abs (x - y) <= tol

class Falsey a where
 bool :: a -> Bool

instance Falsey Double where
 bool x = not $ isClose x 0

instance Falsey Int where
 bool = (/= 0)

instance Falsey Constant where
 bool (StringV s) = not . T.null $ s
 bool (NumV x) = go x where
  go (FloatV v) = bool v
  go (IntV v) = bool v

instance Show Expr where
 show (BinFunc _ e1 e2) = "BinFunc (" ++ show e1 ++ ")" ++ " (" ++ show e2 ++ ")"
 show (Var v) = show v
 show (Rnd e) = "Rnd (" ++ show e ++ ")"
 show (Const c) = show c
 show (UnFunc f e) = "UnFunc (" ++ show e ++ ")"

toInt :: Constant -> Constant
toInt (StringV v) = NumV . IntV . read . T.unpack $ v
toInt (NumV v) = NumV $ go v where
 go (FloatV x) = IntV . round $ x
 go x = x

add _ (StringV _) = undefined
add (StringV _) _  = undefined
add (NumV x) (NumV y) = NumV $ go x y where
 go (IntV x) (IntV y) = IntV $ y + x
 go (FloatV x) (FloatV y) = FloatV $ x + y
 go (FloatV x) (IntV y) = FloatV $ x + (fromIntegral y)
 go (IntV x) (FloatV y) = FloatV $ (fromIntegral x) + y


-- Parse an experssion per the grammer
exprP :: Parser Expr
exprP = do
 e1 <- andExpr
 spaces
 (do
  str "or"
  spaces
  e2 <- exprP
  pure $ BinFunc (\x y -> NumV . IntV . fromEnum $ bool x || bool y) e1 e2
  ) <|> pure e1
andExpr = do
 spaces
 e1 <- notExpr
 spaces
 (do
  str "and"
  spaces
  e2 <- andExpr
  pure $ BinFunc (\x y -> NumV . IntV . fromEnum $ bool x && bool y) e1 e2
  ) <|> pure e1
notExpr :: Parser Expr
notExpr = do
 spaces
 ((UnFunc (NumV . IntV . fromEnum . not . bool)) <$> try (str "not" >> spaces >> cmpExpr)) <|> cmpExpr
cmpExpr = do
 spaces
 e1 <- addExp
 spaces
 (do
  let go (s, f) = let p = try $ str s in (p >> pure f)
  f <- choice . map go $ [("=", (==)), ("<>", (/=)), (">=", (>=)), (">", (>)), ("<=", (<=)), ("<", (<))]
  spaces
  e2 <- cmpExpr
  pure $ BinFunc (\x y -> NumV . IntV . fromEnum $ f x y) e1 e2
  ) <|> pure e1
addExp = do
 spaces
 e1 <- mulExp
 spaces
 (do
  c <- (True <$ str "+" ) <|> (False <$ str "-")
  spaces
  e2 <- addExp
  let fs = ((+), (+), (-), (-))
  pure $ BinFunc (apply fs c) e1 e2
  ) <|> pure e1
mulExp = do
 spaces
 e1 <- negateExpr
 spaces
 (do
  c <- (True <$ str "*" ) <|> (False <$ str "/")
  spaces
  e2 <- mulExp
  let fs = ((*), (*), (div), (/))
  pure $ BinFunc (applyMult fs c) e1 e2
  ) <|> pure e1
negateExpr = do
 spaces
 let
  neg (StringV _) = undefined
  neg (NumV x) = NumV $ go x where
   go (FloatV v) = FloatV $ negate v
   go (IntV v) = IntV $ negate v
 ((UnFunc neg) <$> (str "-" >> powExpr)) <|> powExpr

powExpr = do
 spaces
 e1 <- value
 spaces
 (do
  str "^"
  let fs = ((^), (**), (^^))
  spaces
  e2 <- powExpr
  pure $ BinFunc (applyPow fs) e1 e2
  ) <|> pure e1

var :: Parser Variable
var = do
 c <- letter
 spaces
 es <- option [] (do
  str "("
  spaces
  es <- sepBy1 exprP (spaces >> str ",")
  str ")"
  return es
  )
 pure . Variable c $ es

value = spaces *> (subExpr <|> try func <|> constant <|> (Var <$> var)) where
 subExpr = str "(" *> spaces *> exprP <* spaces <* str ")"
 
 func = do
  c <- (True <$ (try $ str "int")) <|> (False <$ str "rnd")
  spaces
  str "("
  spaces
  e <- exprP
  spaces
  str ")"
  if c then (pure . UnFunc (toInt) $ e) else pure . Rnd $ e
 
 constant = ((Const . NumV . IntV) <$> integer) <|> ((Const . StringV) <$> strP)

strP :: Parser Str
strP = do
 char '"'
 s <- many (satisfy (/= '"'))
 char '"'
 pure . T.pack $ s
   

integer :: Parser Int
integer = go 0 where
 go :: Int -> Parser Int
 go acc = do
  c <- digit
  go' (10 * acc + digitToInt c)
 go' acc = go acc <|> pure acc



stment :: Parser Statement
stment = spaces *> (choice . map try $ [dimP, end, forP, gotoP, gosubP, ifP, inputP, letP, nextP, onP, printP, remP, retP, varP])

dimP = do
 str "dim"
 spaces
 vs <- sepBy1 var (spaces >> str "," >> spaces)
 pure . Dim $ vs


end = End <$ str "end"

forP = do
 str "for"
 spaces
 c <- letter
 spaces
 str "="
 spaces
 e1 <- exprP
 spaces
 str "to"
 spaces
 e2 <- exprP
 spaces
 ((For c e1 e2 . Just) <$> (str "step" >> spaces >> exprP)) <|> pure (For c e1 e2 Nothing)

gotoP = do
 str "goto"
 spaces
 Goto <$> integer
gosubP = do
 str "gosub"
 spaces
 Gosub <$> integer
ifP = do
 str "if"
 spaces
 e <- exprP
 spaces
 str "then"
 spaces
 i <- integer
 pure $ If e i

inputP = do
 str "input"
 spaces
 s <- (strP <|> (pure T.empty))
 spaces
 str ";"
 spaces
 (Input s) <$> (sepBy letter (spaces >> str "," >> spaces))

letP = do
 str "let"
 spaces
 varP
 {-
 e1 <- var
 spaces
 str "="
 e2 <- exprP
 pure $ Let e1 e2
 -}


nextP = do
 str "next"
 spaces
 (Next) <$> (sepBy letter (spaces >> str "," >> spaces))

onP = do
 str "on"
 spaces
 e <- exprP
 str "goto"
 spaces
 xs <- sepBy integer (spaces >> str "," >> spaces)
 pure $ On e xs

printP = do
 str "print"
 spaces
 e <- (do
  try $ str "tab"
  spaces
  str "("
  spaces
  e <- exprP
  spaces
  str ")"
  spaces
  oneOf ",;"
  pure . pure $ e
  ) <|> (pure Nothing)
 es <- sepBy exprP (spaces >> oneOf ",;")
 pure $ Print e es

remP = do
 str "rem"
 spaces
 skipMany alphaNum
 pure Rem

retP = Ret <$ str "return"

varP = do
 e1 <- var
 spaces
 str "="
 e2 <- exprP
 pure $ Let e1 e2