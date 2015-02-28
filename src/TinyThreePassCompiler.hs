{-# LANGUAGE TemplateHaskell #-}
module TinyThreePassCompiler where

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (listToMaybe)
import Control.Lens
import Control.Applicative
import Data.Maybe (fromJust)
import Data.List (isPrefixOf, (!!))


data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs


------------------------------------------------------------------------------
-- | Parser.
data ParserSate = ParserSate { _input     :: [ Token ]
                             , _pos       :: Int
                             , _variables :: M.Map String Int
                             }
makeLenses ''ParserSate
type Parser a = (StateT ParserSate Maybe) a


runParser :: Parser a -> [Token] -> Maybe a
runParser p i = evalStateT p (ParserSate i 0 M.empty)


next :: Parser Token
next = do
  t <- liftM listToMaybe $ use input
  input %= tail
  lift t


token :: Token -> Parser ()
token (TChar x) = next >>= \t -> guard $ t == TChar x
token _         = undefined


variable :: Parser ()
variable = next >>= \t -> case t of
  (TStr name) -> do
    p <- pos <%= succ
    variables %= M.insert name (p - 1)
  _           -> mzero


value :: Parser AST
value = immVal <|> varVal
  where
    immVal = next >>= \t -> case t of
      (TInt i) -> return $ Imm i
      _        -> mzero
    varVal = next >>= \t -> case t of
      (TStr name) -> do
        p <- liftM (M.lookup name) $ use variables
        lift $ Arg <$> p
      _           -> mzero


operator :: Parser AST -> Token -> Parser AST -> Parser AST
operator l (TChar o) r = do
  l' <- l
  token (TChar o)
  r' <- r
  return $ toAST o l' r'
operator _ _ _ = undefined


toAST :: Char -> (AST -> AST -> AST)
toAST '+' = Add
toAST '-' = Sub
toAST '*' = Mul
toAST '/' = Div
toAST _   = undefined


pass1 :: String -> AST
pass1 = fromJust . runParser function . tokenize
  where
    function      = do
      token (TChar '[') *> argument_list <* token (TChar ']')
      expression
    argument_list = void $ many variable
    expression    = operator term (TChar '+') expression
                    <|> operator term (TChar '-') expression
                    <|> term
    term          = operator factor (TChar '*') term
                    <|> operator factor (TChar '/') term
                    <|> factor
    factor        = token (TChar '(') *> expression <* token (TChar ')')
                    <|> value


------------------------------------------------------------------------------
-- Optimizer
pass2 :: AST -> AST
pass2 (Imm x) = Imm x
pass2 (Arg x) = Arg x
pass2 (Add x1 x2) = case ((pass2 x1), (pass2 x2)) of
  (Imm a, Imm b) -> Imm $ a + b
  (l, r)         -> Add l r
pass2 (Sub x1 x2) = case ((pass2 x1), (pass2 x2)) of
  (Imm a, Imm b) -> Imm $ a - b
  (l, r)         -> Sub l r
pass2 (Mul x1 x2) = case ((pass2 x1), (pass2 x2)) of
  (Imm a, Imm b) -> Imm $ a * b
  (l, r)         -> Mul l r
pass2 (Div x1 x2) = case ((pass2 x1), (pass2 x2)) of
  (Imm a, Imm b) -> Imm $ a `div` b
  (l, r)         -> Div l r  


------------------------------------------------------------------------------
-- Code generator

------------------------------------------------------------------------------
-- | Intruction set
data Instruction = IM Int
                 | AR Int
                 | PU | PO | SW | AD | SU | MU | DI deriving (Eq, Show)


class Match a where
  match :: a -> a -> Bool


instance Match Instruction where
  match (IM _) (IM _) = True
  match (IM _) (AR _) = True
  match (AR _) (IM _) = True
  match (AR _) (AR _) = True  
  match  x y          = x == y


instance Match a => Match [a] where
  match a b = and $ zipWith match a b


generate :: AST -> [ Instruction ]
generate (Imm x) = [ IM x, PU ]
generate (Arg x) = [ AR x, PU ]
generate (Add x1 x2) = (generate x1) ++ (generate x2) ++ popPush AD
generate (Sub x1 x2) = (generate x1) ++ (generate x2) ++ popPush SU
generate (Mul x1 x2) = (generate x1) ++ (generate x2) ++ popPush MU
generate (Div x1 x2) = (generate x1) ++ (generate x2) ++ popPush DI


popPush :: Instruction -> [Instruction]
popPush x = [ PO, SW, PO, x, PU ]


replace :: Match t => [t] -> ([t] -> [t]) -> [t] -> [t]
replace _ _ [] = []
replace old rw l@(x:xs)
  | old `match` l = let (a, b) = splitAt (length old) l
                    in (rw a) ++ replace old rw b
  | otherwise     = x:(replace old rw xs)


peepHole :: [Instruction] -> [Instruction]
peepHole = replace [ PU, IM undefined, SW, PO] (\t -> [SW, t !! 1])
           . replace [ PU, PO, SW ] (const [ SW ])

pass3 :: AST -> [ String ]
pass3 = map show . peepHole . init . generate


compile :: String -> [String]
compile = pass3 . pass2 . pass1

