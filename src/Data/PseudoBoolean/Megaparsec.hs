{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PseudoBoolean.Megaparsec
-- Copyright   :  (c) Masahiro Sakai 2011-2016
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Portability :  non-portable (BangPatterns, FlexibleContexts, TypeFamilies, CPP, ConstraintKinds)
--
-- A parser library for OPB file and WBO files used in pseudo boolean competition.
-- 
-- References:
--
-- * Input/Output Format and Solver Requirements for the Competitions of
--   Pseudo-Boolean Solvers
--   <http://www.cril.univ-artois.fr/PB11/format.pdf>
--
-----------------------------------------------------------------------------

module Data.PseudoBoolean.Megaparsec
  (
  -- * Parsing OPB files
    opbParser
  , parseOPBString
  , parseOPBByteString
  , parseOPBFile

  -- * Parsing WBO files
  , wboParser
  , parseWBOString
  , parseWBOByteString
  , parseWBOFile

  -- * Error type
  , ParseError
  ) where

import Prelude hiding (sum)
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Word
import Data.Void
import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Byte
import Data.PseudoBoolean.Types
import Data.PseudoBoolean.Internal.TextUtil

type C e s m = (MonadParsec e s m, Token s ~ Word8, IsString (Tokens s))

char8 :: C e s m => Char -> m Word8
char8 = char . fromIntegral . fromEnum

anyChar :: C e s m => m Word8
anyChar = anySingle

-- | Parser for OPB files
opbParser :: (MonadParsec e s m, Token s ~ Word8, IsString (Tokens s)) => m Formula
opbParser = formula

-- | Parser for WBO files
wboParser :: (MonadParsec e s m, Token s ~ Word8, IsString (Tokens s)) => m SoftFormula
wboParser = softformula

-- <formula>::= <sequence_of_comments> [<objective>] <sequence_of_comments_or_constraints>
formula :: C e s m => m Formula
formula = do
  h <- optional hint
  sequence_of_comments
  obj <- optional objective
  cs <- sequence_of_comments_or_constraints
  return $
    Formula
    { pbObjectiveFunction = obj
    , pbConstraints = cs
    , pbNumVars = fromMaybe (pbComputeNumVars (fmap snd obj) cs) (fmap fst h)
    , pbNumConstraints = fromMaybe (length cs) (fmap snd h)
    }

hint :: C e s m => m (Int,Int)
hint = try $ do
  _ <- char8 '*'
  zeroOrMoreSpace
  _ <- string "#variable="
  zeroOrMoreSpace
  nv <- unsigned_integer
  oneOrMoreSpace  
  _ <- string "#constraint="
  zeroOrMoreSpace
  nc <- unsigned_integer
  _ <- manyTill anyChar eol
  return (fromIntegral nv, fromIntegral nc)

-- <sequence_of_comments>::= <comment> [<sequence_of_comments>]
sequence_of_comments :: C e s m => m ()
sequence_of_comments = skipMany comment -- XXX: we allow empty sequence

-- <comment>::= "*" <any_sequence_of_characters_other_than_EOL> <EOL>
comment :: C e s m => m ()
comment = do
  _ <- char8 '*' 
  _ <- manyTill anyChar eol
  space -- We relax the grammer and allow spaces in the beggining of next component.
  return ()

-- <sequence_of_comments_or_constraints>::= <comment_or_constraint> [<sequence_of_comments_or_constraints>]
sequence_of_comments_or_constraints :: C e s m => m [Constraint]
sequence_of_comments_or_constraints = do
  xs <- many comment_or_constraint -- We relax the grammer and allow spaces in the beginning of next component.
  return $ catMaybes xs

-- <comment_or_constraint>::= <comment>|<constraint>
comment_or_constraint :: C e s m => m (Maybe Constraint)
comment_or_constraint =
  (comment >> return Nothing) <|> (liftM Just constraint)

--  <objective>::= <objective_type> <zeroOrMoreSpace> <sum> ";"
objective :: C e s m => m Objective
objective = do
  dir <- objective_type
  zeroOrMoreSpace
  obj <- sum
  semi
  return (dir, obj)

-- <constraint>::= <sum> <relational_operator> <zeroOrMoreSpace> <integer> <zeroOrMoreSpace> ";"
constraint :: C e s m => m Constraint
constraint = do
  lhs <- sum
  op <- relational_operator
  zeroOrMoreSpace
  rhs <- integer
  zeroOrMoreSpace
  semi
  return (lhs, op, rhs)

-- <sum>::= <weightedterm> | <weightedterm> <sum>
sum :: C e s m => m Sum
sum = many weightedterm -- we relax the grammer to allow empty sum

-- <weightedterm>::= <integer> <oneOrMoreSpace> <term> <oneOrMoreSpace>
weightedterm :: C e s m => m WeightedTerm
weightedterm = do
  w <- integer
  oneOrMoreSpace
  t <- term
  zeroOrMoreSpace -- we relax the grammar to allow omitting spaces at the end of <sum>.
  return (w,t)

-- <integer>::= <unsigned_integer> | "+" <unsigned_integer> | "-" <unsigned_integer>
integer :: C e s m => m Integer
integer = msum
  [ unsigned_integer
  , char8 '+' >> unsigned_integer
  , char8 '-' >> liftM negate unsigned_integer
  ]

-- <unsigned_integer>::= <digit> | <digit><unsigned_integer>
unsigned_integer :: C e s m => m Integer
unsigned_integer = do
  ds <- some digitChar
  return $! readUnsignedInteger (map (toEnum . fromIntegral) ds)

-- <objective_type>::= "min:" | "max:"
objective_type :: C e s m => m OptDir
objective_type = (try (string "min:") >> return OptMin) <|> (string "max:" >> return OptMax)

-- <relational_operator>::= ">=" | "="
relational_operator :: forall e s m. C e s m => m Op
relational_operator = msum $ map try
  [ string "=" >> return Eq
  , string "!=" >> return NEq
  , string ">=" >> return Ge
  , string ">" >> return Gt
  , string "<=" >> return Le
  , string "<" >> return Lt
  , u8string "≠" >> return NEq
  , u8string "≥" >> return Ge
  , u8string "≤" >> return Le
  ]
  where
    -- XXX: We cannot assume Tokens s ~ ByteString
    u8string = string . tokensToChunk (Proxy :: Proxy s) . BL.unpack  . UTF8.fromString

-- <variablename>::= "x" <unsigned_integer>
variablename :: C e s m => m Var
variablename = do
  _ <- char8 'x'
  i <- unsigned_integer
  return $! fromIntegral i

-- <oneOrMoreSpace>::= " " [<oneOrMoreSpace>]
oneOrMoreSpace :: C e s m => m ()
oneOrMoreSpace  = skipSome (char8 ' ')

-- <zeroOrMoreSpace>::= [" " <zeroOrMoreSpace>]
zeroOrMoreSpace :: C e s m => m ()
-- zeroOrMoreSpace = skipMany (char8 ' ')
zeroOrMoreSpace = space
-- We relax the grammer and allow more type of spacing

semi :: C e s m => m ()
semi = char8 ';' >> space
-- We relax the grammer and allow spaces in the beginning of next component.

{-
For linear pseudo-Boolean instances, <term> is defined as
<term>::=<variablename>

For non-linear instances, <term> is defined as
<term>::= <oneOrMoreLiterals>
-}
term :: C e s m => m Term
term = oneOrMoreLiterals

-- <oneOrMoreLiterals>::= <literal> | <literal> <oneOrMoreSpace> <oneOrMoreLiterals>
oneOrMoreLiterals :: C e s m => m [Lit]
oneOrMoreLiterals = do
  l <- literal
  mplus (try $ oneOrMoreSpace >> liftM (l:) oneOrMoreLiterals) (return [l])
-- Note that we cannot use sepBy1.
-- In "p `sepBy1` q", p should success whenever q success.
-- But it's not the case here.

-- <literal>::= <variablename> | "~"<variablename>
literal :: C e s m => m Lit
literal = variablename <|> (char8 '~' >> liftM negate variablename)

type ParseError = MP.ParseErrorBundle BL.ByteString Void

-- | Parse a OPB format string containing pseudo boolean problem.
parseOPBString :: String -> String -> Either ParseError Formula
parseOPBString info s = parse (formula <* eof) info (UTF8.fromString s)

-- | Parse a OPB format lazy bytestring containing pseudo boolean problem.
parseOPBByteString :: String -> ByteString -> Either ParseError Formula
parseOPBByteString = parse (formula <* eof)

-- | Parse a OPB file containing pseudo boolean problem.
parseOPBFile :: FilePath -> IO (Either ParseError Formula)
parseOPBFile filepath = do
  s <- BL.readFile filepath
  return $! parse (formula <* eof) filepath s

-- <softformula>::= <sequence_of_comments> <softheader> <sequence_of_comments_or_constraints>
softformula :: C e s m => m SoftFormula
softformula = do
  h <- optional hint
  sequence_of_comments
  top <- softheader
  cs <- wbo_sequence_of_comments_or_constraints
  return $
    SoftFormula
    { wboTopCost = top
    , wboConstraints = cs
    , wboNumVars = fromMaybe (wboComputeNumVars cs) (fmap fst h)
    , wboNumConstraints = fromMaybe (length cs) (fmap snd h)
    }

-- <softheader>::= "soft:" [<unsigned_integer>] ";"
softheader :: C e s m => m (Maybe Integer)
softheader = do
  _ <- string "soft:"
  zeroOrMoreSpace -- XXX
  top <- optional unsigned_integer
  zeroOrMoreSpace -- XXX
  semi
  return top

-- <sequence_of_comments_or_constraints>::= <comment_or_constraint> [<sequence_of_comments_or_constraints>]
wbo_sequence_of_comments_or_constraints :: C e s m => m [SoftConstraint]
wbo_sequence_of_comments_or_constraints = do
  xs <- many wbo_comment_or_constraint -- XXX: we relax the grammer to allow empty sequence
  return $ catMaybes xs

-- <comment_or_constraint>::= <comment>|<constraint>|<softconstraint>
wbo_comment_or_constraint :: C e s m => m (Maybe SoftConstraint)
wbo_comment_or_constraint = (comment >> return Nothing) <|> m
  where
    m = liftM Just $ (constraint >>= \c -> return (Nothing, c)) <|> softconstraint

-- <softconstraint>::= "[" <zeroOrMoreSpace> <unsigned_integer> <zeroOrMoreSpace> "]" <constraint>
softconstraint :: C e s m => m SoftConstraint
softconstraint = do
  _ <- char8 '['
  zeroOrMoreSpace
  cost <- unsigned_integer
  zeroOrMoreSpace
  _ <- char8 ']'
  zeroOrMoreSpace -- XXX
  c <- constraint
  return (Just cost, c)

-- | Parse a WBO format string containing weighted boolean optimization problem.
parseWBOString :: String -> String -> Either ParseError SoftFormula
parseWBOString info s = parse (softformula <* eof) info (UTF8.fromString s)

-- | Parse a WBO format lazy bytestring containing pseudo boolean problem.
parseWBOByteString :: String -> ByteString -> Either ParseError SoftFormula
parseWBOByteString = parse (softformula <* eof)

-- | Parse a WBO file containing weighted boolean optimization problem.
parseWBOFile :: FilePath -> IO (Either ParseError SoftFormula)
parseWBOFile filepath = do
  s <- BL.readFile filepath
  return $! parse (softformula <* eof) filepath s
