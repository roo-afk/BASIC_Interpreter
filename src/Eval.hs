module Main where

{-# LANGUAGE BangPatterns #-}

import Lex hiding (value)
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (fromJust, fromMaybe)
import System.Random
import Text.Parsec (parse)
import Data.Char (isDigit)
import Debug.Trace (trace)
import Control.Monad (replicateM)
import System.IO (hFlush, stdout)


-- import Text.Parsec
import qualified Data.Text.IO as TIO (putStr, readFile)
import qualified Data.Text as T



data SymbolVal = Val {value :: Constant, retVal :: Maybe Int, endVal :: Maybe Constant, step :: (Constant -> Constant)} 
 | Nested (M.Map Int SymbolVal)
type SymbolTable = M.Map Char SymbolVal


instance Show SymbolVal where
 show (Val v ret end _) = "Val:" ++ show v
 show (Nested m) = show m



create' :: [Int] -> SymbolVal
create' [] = Val (NumV $ IntV 0) (Nothing) (Nothing) id
create' (i:is) = Nested $ M.fromList $ zip [0..i] (repeat $ create' is)


type Env = M.Map Int [Statement]

type Eval = StateT SymbolTable IO


evalExpr :: Expr -> Int -> Eval Constant
evalExpr (Const x) _ = pure x
evalExpr (BinFunc f e1 e2) pc = f <$> (evalExpr e1 pc) <*> (evalExpr e2 pc)
evalExpr (UnFunc f e) pc = f <$> (evalExpr e pc)
evalExpr (Rnd e) pc = do
 c <- evalExpr e pc
 let
  go :: Number -> Eval Number
  go (IntV 1) = FloatV <$> (liftIO $ (randomRIO (0, 1) :: IO Double)) 
  go (IntV x) = IntV <$> (liftIO $ randomRIO (0, x - 1))
  go v@(FloatV _) = let (NumV v') = toInt (NumV v) in go v'
 case c of
   s@(StringV _) -> pure $ toInt s
   (NumV v) -> (NumV) <$> go v
evalExpr (Var v@(Variable c _)) pc = lookupSymbol v pc >>= \x -> case x of
 Just (y@(Val _ _ _ _)) -> pure $ value y
 x -> error $ "Symbol: " ++ [c] ++ " not found" ++ " at: " ++ show pc 
 


lookupSymbol :: Variable -> Int -> Eval (Maybe SymbolVal)
lookupSymbol (Variable c es) pc = do 
 symTable <- get
 let 
  go x@(Val _ _ _ _) _ = pure $ pure x 
  go m@(Nested _) [] = pure $ pure m
  go (Nested m) (e:es) = do
   c <- evalExpr e pc
   let 
    c' = case c of
     (StringV _) -> -1
     (NumV (FloatV _)) -> -1
     (NumV (IntV x)) -> x
   case M.lookup c' m of
    Nothing -> pure Nothing
    (Just m') -> go m' es 
 case M.lookup c symTable of
  Nothing -> pure Nothing
  (Just res) -> go res es


updateSymbol :: [Int] -> Char -> Constant -> Maybe Int -> Maybe Constant -> (Constant -> Constant) -> SymbolTable -> SymbolTable
updateSymbol is c !val ret end f symTable = M.alter (go is) c symTable where
 go [] _ = pure $ Val {value = val, retVal = ret, endVal = end, step = f}
 go (i:is) mVal = do
  v <- mVal
  case v of
   (Nested m') -> pure $ Nested $ M.alter (go is) i m'
   _ -> pure $ Val (val) (ret) (end) (f)


evalSts :: Env -> Int -> Eval Bool
evalSts env !pc = evalSts' env pc (fromMaybe ([]) $ M.lookup pc env) where
 evalSts' :: Env -> Int -> [Statement] -> Eval Bool
 evalSts' env pc [] = case M.lookupGT pc env of
  Nothing -> error $ "Line number: " ++ show pc ++ " does not exist"
  (Just (pc', ss')) -> evalSts' env pc' ss'
 evalSts' env pc (s:ss) = go s where
  go (End) = pure True
  -- Finish Later
  go (For c e1 e2 me3) =
   do
    c1 <- evalExpr e1 pc
    c2 <- evalExpr e2 pc
    x <- case me3 of Nothing -> pure $ NumV $ IntV 1; (Just e3) -> evalExpr e3 pc
    modify $ updateSymbol [] c c1 (fst <$> M.lookupGT pc env) (Just c2) (add x)
    evalSts' env pc ss
  go (Goto i) = evalSts env i
  go (Gosub i) = evalSts env i >>= \c -> if c then pure True else evalSts' env pc ss
  go (If e i) = evalExpr e pc >>= \c -> if bool c then evalSts env i else evalSts' env pc ss
  go (Input s ids) = do
   liftIO $ (TIO.putStr s >> putChar ' ' >> hFlush stdout)
   let
    go :: Char -> Eval ()
    go c = do
     i <- liftIO (read <$> getLine)
     modify $ updateSymbol [] c (NumV . IntV $ i) Nothing Nothing id
   mapM_ (go) ids
   evalSts' env pc ss
  go (Let (Variable c es) e) = do
   is <- mapM (fmap (\(NumV (IntV v)) -> v) . fmap toInt . (`evalExpr` pc)) es
   c2 <- evalExpr e pc
   modify $ updateSymbol is c c2 Nothing Nothing id
   evalSts' env pc ss
  go (Next ids) = foldr go' (evalSts' env pc ss) ids where
   go' c acc = do
    x <- lookupSymbol (Variable c []) pc
    case x of
     Just (Val v ret end f) -> if Just v == end then acc else ((modify $ updateSymbol [] c (f v) ret end f) >> evalSts env (fromJust ret))
     _ -> error $ "Wrong value for symbol: " ++ [c] ++ " at line: " ++ show pc
    
  go (Ret) = pure False
  go Rem = evalSts' env pc []
  go (Print _ []) = (liftIO $ putChar '\n') >> evalSts' env pc ss
  go (Print Nothing es) = foldr printVals (evalSts' env pc ss) es where
   printVals e acc = do
    c <- evalExpr e pc
    liftIO . (>> putChar ' ') $ case c of (StringV s) -> TIO.putStr s; x -> putStr $ show x
    acc
  go (Print (Just e) es) = do
   c1 <- evalExpr e pc
   let (NumV (IntV x)) = toInt c1
   liftIO . putStr . replicate x $ '\t'
   go (Print Nothing es)
  go (Dim vs) = foldr create (evalSts' env pc ss) vs where
   create (Variable c es) acc = do
    is <- mapM (fmap (\(NumV (IntV v)) -> v) . fmap toInt . (`evalExpr` pc)) es
    modify $ M.insert c (create' is)
    acc




main :: IO ()
main = do
 setStdGen (mkStdGen 15)
 putStr "What is the filename: "
 hFlush stdout
 file <- getLine
 texts <- (T.lines . T.toLower) <$> TIO.readFile file
 let 
  f n s = case (parse stment "" s) of
   (Left err) -> error $ "Parse Error at line: " ++ show n ++ " at statment " ++ show err
   (Right v) -> v
 let env = M.fromList . map ((\(n, ss) -> (read $ T.unpack n :: Int, map (f n) $ T.split (== ':') $ T.dropWhile (== ' ') ss)) . T.span isDigit) $ texts
 let (key, val) = (M.findMin env)
 -- print env
 -- let f (Val val ret end _) acc = (mapM_ ((>> putChar ' ') . putStr) [show val,show ret, show end, "\n"]) >> acc
 evalStateT (evalSts env key) M.empty
 putChar '\n'








