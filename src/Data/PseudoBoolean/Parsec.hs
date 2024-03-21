{-# LANGUAGE BangPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PseudoBoolean.Parsec
-- Copyright   :  (c) Masahiro Sakai 2011-2015
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Portability :  non-portable (BangPatterns, FlexibleContexts)
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

module Data.PseudoBoolean.Parsec
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
  ) where

import Prelude hiding (sum)
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import Text.Parsec
import qualified Text.Parsec.ByteString.Lazy as ParsecBS
import Data.PseudoBoolean.Types
import Data.PseudoBoolean.Internal.TextUtil

-- | Parser for OPB files
opbParser :: Stream s m Char => ParsecT s u m Formula
opbParser = formula

-- | Parser for WBO files
wboParser :: Stream s m Char => ParsecT s u m SoftFormula
wboParser = softformula

-- <formula>::= <sequence_of_comments> [<objective>] <sequence_of_comments_or_constraints>
formula :: Stream s m Char => ParsecT s u m Formula
formula = do
  h <- optionMaybe hint
  sequence_of_comments
  obj <- optionMaybe objective
  cs <- sequence_of_comments_or_constraints
  return $
    Formula
    { pbObjectiveFunction = obj
    , pbConstraints = cs
    , pbNumVars = fromMaybe (pbComputeNumVars (fmap snd obj) cs) (fmap fst h)
    , pbNumConstraints = fromMaybe (length cs) (fmap snd h)
    }

hint :: Stream s m Char => ParsecT s u m (Int,Int)
hint = try $ do
  _ <- char '*'
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
sequence_of_comments :: Stream s m Char => ParsecT s u m ()
sequence_of_comments = skipMany comment -- XXX: we allow empty sequence

-- <comment>::= "*" <any_sequence_of_characters_other_than_EOL> <EOL>
comment :: Stream s m Char => ParsecT s u m ()
comment = do
  _ <- char '*' 
  _ <- manyTill anyChar eol
  spaces -- We relax the grammer and allow spaces in the beggining of next component.
  return ()

-- <sequence_of_comments_or_constraints>::= <comment_or_constraint> [<sequence_of_comments_or_constraints>]
sequence_of_comments_or_constraints :: Stream s m Char => ParsecT s u m [Constraint]
sequence_of_comments_or_constraints = do
  xs <- many comment_or_constraint -- We relax the grammer and allow spaces in the beginning of next component.
  return $ catMaybes xs

-- <comment_or_constraint>::= <comment>|<constraint>
comment_or_constraint :: Stream s m Char => ParsecT s u m (Maybe Constraint)
comment_or_constraint =
  (comment >> return Nothing) <|> (liftM Just constraint)

-- <objective>::= <objective_type> <zeroOrMoreSpace> <sum> ";"
objective :: Stream s m Char => ParsecT s u m Objective
objective = do
  dir <- objective_type
  zeroOrMoreSpace
  obj <- sum
  semi
  return (dir, obj)

-- <constraint>::= <sum> <relational_operator> <zeroOrMoreSpace> <integer> <zeroOrMoreSpace> ";"
constraint :: Stream s m Char => ParsecT s u m Constraint
constraint = do
  lhs <- sum
  op <- relational_operator
  zeroOrMoreSpace
  rhs <- integer
  zeroOrMoreSpace
  semi
  return (lhs, op, rhs)

-- <sum>::= <weightedterm> | <weightedterm> <sum>
sum :: Stream s m Char => ParsecT s u m Sum
sum = many weightedterm -- we relax the grammer to allow empty sum

-- <weightedterm>::= <integer> <oneOrMoreSpace> <term> <oneOrMoreSpace>
weightedterm :: Stream s m Char => ParsecT s u m WeightedTerm
weightedterm = do
  w <- integer
  oneOrMoreSpace
  t <- term
  zeroOrMoreSpace -- we relax the grammar to allow omitting spaces at the end of <sum>.
  return (w,t)

-- <integer>::= <unsigned_integer> | "+" <unsigned_integer> | "-" <unsigned_integer>
integer :: Stream s m Char => ParsecT s u m Integer
integer = msum
  [ unsigned_integer
  , char '+' >> unsigned_integer
  , char '-' >> liftM negate unsigned_integer
  ]

-- <unsigned_integer>::= <digit> | <digit><unsigned_integer>
unsigned_integer :: Stream s m Char => ParsecT s u m Integer
unsigned_integer = do
  ds <- many1 digit
  return $! readUnsignedInteger ds

-- <objective_type>::= "min:" | "max:"
objective_type :: Stream s m Char => ParsecT s u m OptDir
objective_type = (try (string "min:") >> return OptMin) <|> (string "max:" >> return OptMax)

-- <relational_operator>::= ">=" | "="
relational_operator :: Stream s m Char => ParsecT s u m Op
relational_operator = (string ">=" >> return Ge) <|> (string "=" >> return Eq)

-- <variablename>::= "x" <unsigned_integer>
variablename :: Stream s m Char => ParsecT s u m Var
variablename = do
  _ <- char 'x'
  i <- unsigned_integer
  return $! fromIntegral i

-- <oneOrMoreSpace>::= " " [<oneOrMoreSpace>]
oneOrMoreSpace :: Stream s m Char => ParsecT s u m ()
oneOrMoreSpace  = skipMany1 (char ' ')

-- <zeroOrMoreSpace>::= [" " <zeroOrMoreSpace>]
zeroOrMoreSpace :: Stream s m Char => ParsecT s u m ()
-- zeroOrMoreSpace = skipMany (char ' ')
zeroOrMoreSpace = spaces
-- We relax the grammer and allow more type of spacing

eol :: Stream s m Char => ParsecT s u m ()
eol = char '\n' >> return ()

semi :: Stream s m Char => ParsecT s u m ()
semi = char ';' >> spaces
-- We relax the grammer and allow spaces in the beginning of next component.

{-
For linear pseudo-Boolean instances, <term> is defined as
<term>::=<variablename>

For non-linear instances, <term> is defined as
<term>::= <oneOrMoreLiterals>
-}
term :: Stream s m Char => ParsecT s u m Term
term = oneOrMoreLiterals

-- <oneOrMoreLiterals>::= <literal> | <literal> <oneOrMoreSpace> <oneOrMoreLiterals>
oneOrMoreLiterals :: Stream s m Char => ParsecT s u m [Lit]
oneOrMoreLiterals = do
  l <- literal
  mplus (try $ oneOrMoreSpace >> liftM (l:) oneOrMoreLiterals) (return [l])
-- Note that we cannot use sepBy1.
-- In "p `sepBy1` q", p should success whenever q success.
-- But it's not the case here.

-- <literal>::= <variablename> | "~"<variablename>
literal :: Stream s m Char => ParsecT s u m Lit
literal = variablename <|> (char '~' >> liftM negate variablename)

-- | Parse a OPB format string containing pseudo boolean problem.
parseOPBString :: SourceName -> String -> Either ParseError Formula
parseOPBString = parse (formula <* eof)

-- | Parse a OPB format lazy bytestring containing pseudo boolean problem.
parseOPBByteString :: SourceName -> ByteString -> Either ParseError Formula
parseOPBByteString = parse (formula <* eof)

-- | Parse a OPB file containing pseudo boolean problem.
parseOPBFile :: FilePath -> IO (Either ParseError Formula)
parseOPBFile = ParsecBS.parseFromFile (formula <* eof)


-- <softformula>::= <sequence_of_comments> <softheader> <sequence_of_comments_or_constraints>
softformula :: Stream s m Char => ParsecT s u m SoftFormula
softformula = do
  h <- optionMaybe hint
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
softheader :: Stream s m Char => ParsecT s u m (Maybe Integer)
softheader = do
  _ <- string "soft:"
  zeroOrMoreSpace -- XXX
  top <- optionMaybe unsigned_integer
  zeroOrMoreSpace -- XXX
  semi
  return top

-- <sequence_of_comments_or_constraints>::= <comment_or_constraint> [<sequence_of_comments_or_constraints>]
wbo_sequence_of_comments_or_constraints :: Stream s m Char => ParsecT s u m [SoftConstraint]
wbo_sequence_of_comments_or_constraints = do
  xs <- many wbo_comment_or_constraint -- XXX: we relax the grammer to allow empty sequence
  return $ catMaybes xs

-- <comment_or_constraint>::= <comment>|<constraint>|<softconstraint>
wbo_comment_or_constraint :: Stream s m Char => ParsecT s u m (Maybe SoftConstraint)
wbo_comment_or_constraint = (comment >> return Nothing) <|> m
  where
    m = liftM Just $ (constraint >>= \c -> return (Nothing, c)) <|> softconstraint

-- <softconstraint>::= "[" <zeroOrMoreSpace> <unsigned_integer> <zeroOrMoreSpace> "]" <constraint>
softconstraint :: Stream s m Char => ParsecT s u m SoftConstraint
softconstraint = do
  _ <- char '['
  zeroOrMoreSpace
  cost <- unsigned_integer
  zeroOrMoreSpace
  _ <- char ']'
  zeroOrMoreSpace -- XXX
  c <- constraint
  return (Just cost, c)

-- | Parse a WBO format string containing weighted boolean optimization problem.
parseWBOString :: SourceName -> String -> Either ParseError SoftFormula
parseWBOString = parse (softformula <* eof)

-- | Parse a WBO format lazy bytestring containing pseudo boolean problem.
parseWBOByteString :: SourceName -> ByteString -> Either ParseError SoftFormula
parseWBOByteString = parse (softformula <* eof)

-- | Parse a WBO file containing weighted boolean optimization problem.
parseWBOFile :: FilePath -> IO (Either ParseError SoftFormula)
parseWBOFile = ParsecBS.parseFromFile (softformula <* eof)
