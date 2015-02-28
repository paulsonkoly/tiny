{-# LANGUAGE TemplateHaskell #-}
module TinyThreePassCompiler where

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (listToMaybe)
import Control.Lens
import Control.Applicative
import Data.Maybe (fromJust)


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
-- | Parser state
data ParserSate = ParserSate { _input     :: [ Token ]
                             , _pos       :: Int
                             , _variables :: M.Map String Int
                             }
makeLenses ''ParserSate
type Parser a = (StateT ParserSate Maybe) a


runParser :: Parser a -> [Token] -> Maybe a
runParser p i = evalStateT p (ParserSate i 0 M.empty)


------------------------------------------------------------------------------
-- | next token
next :: Parser Token
next = do
  t <- liftM listToMaybe $ use input
  input %= tail
  lift t


------------------------------------------------------------------------------
-- | Asserts that the next token is the one given
token :: Token -> Parser ()
token (TChar x) = next >>= \t -> guard $ t == TChar x
token _         = undefined


------------------------------------------------------------------------------
-- | Reads a variable name and creates the symbol table entry
variable :: Parser ()
variable = next >>= \t -> case t of
  (TStr name) -> do
    p <- pos <%= succ
    variables %= M.insert name (p - 1)
  _           -> mzero


------------------------------------------------------------------------------
-- | Parses a single value either immediate or a variable reference
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


------------------------------------------------------------------------------
-- | Parses an operator
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


------------------------------------------------------------------------------
-- | Parser
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
instance Plated AST where
  plate f (Add x y) = Add <$> f x <*> f y
  plate f (Sub x y) = Sub <$> f x <*> f y 
  plate f (Mul x y) = Mul <$> f x <*> f y 
  plate f (Div x y) = Div <$> f x <*> f y
  plate _ x         = pure x


------------------------------------------------------------------------------
-- | Simplifier
pass2 :: AST -> AST
pass2 = transform f
  where f (Add (Imm a) (Imm b)) = Imm $ a + b
        f (Sub (Imm a) (Imm b)) = Imm $ a - b
        f (Mul (Imm a) (Imm b)) = Imm $ a * b
        f (Div (Imm a) (Imm b)) = Imm $ a `div` b
        f x = x        


------------------------------------------------------------------------------
-- | Intruction set
data Instruction = IM Int
                 | AR Int
                 | PU | PO | SW | AD | SU | MU | DI deriving (Eq, Show)


generate :: AST -> [ Instruction ]
generate (Imm x) = [ IM x, PU ]
generate (Arg x) = [ AR x, PU ]
generate (Add x1 x2) = (generate x1) ++ (generate x2) ++ popPush AD
generate (Sub x1 x2) = (generate x1) ++ (generate x2) ++ popPush SU
generate (Mul x1 x2) = (generate x1) ++ (generate x2) ++ popPush MU
generate (Div x1 x2) = (generate x1) ++ (generate x2) ++ popPush DI


popPush :: Instruction -> [Instruction]
popPush x = [ PO, SW, PO, x, PU ]


transformPUPOSW :: [Instruction] -> [Instruction]
transformPUPOSW (PU:PO:SW:t) = SW:t
transformPUPOSW x            = x


transformBadSW :: [Instruction] -> [Instruction]
transformBadSW (PU:(IM x):SW:PO:t) = SW:(IM x):t
transformBadSW (PU:(AR x):SW:PO:t) = SW:(AR x):t
transformBadSW x                   = x


peepHole :: [Instruction] -> [Instruction]
peepHole = transform transformBadSW . transform transformPUPOSW


------------------------------------------------------------------------------
-- | Code generator
pass3 :: AST -> [ String ]
pass3 = map show . peepHole . init . generate


compile :: String -> [String]
compile = pass3 . pass2 . pass1
