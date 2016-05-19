{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies, CPP, ConstraintKinds #-}
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
  ) where

import Prelude hiding (sum)
import Control.Applicative ((<*))
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Prim (MonadParsec ())
import Data.PseudoBoolean.Types
import Data.PseudoBoolean.Internal.TextUtil

#if MIN_VERSION_megaparsec(5,0,0)
type C e s m = (MonadParsec e s m, Token s ~ Char)
#else
type C e s m = (MonadParsec s m Char)
#endif

-- | Parser for OPB files
#if MIN_VERSION_megaparsec(5,0,0)
opbParser :: (MonadParsec e s m, Token s ~ Char) => m Formula
#else
opbParser :: (MonadParsec s m Char) => m Formula
#endif
opbParser = formula

-- | Parser for WBO files
#if MIN_VERSION_megaparsec(5,0,0)
wboParser :: (MonadParsec e s m, Token s ~ Char) => m SoftFormula
#else
wboParser :: (MonadParsec s m Char) => m SoftFormula
#endif
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
    , pbNumVars = fromMaybe (pbComputeNumVars obj cs) (fmap fst h)
    , pbNumConstraints = fromMaybe (length cs) (fmap snd h)
    }

hint :: C e s m => m (Int,Int)
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
sequence_of_comments :: C e s m => m ()
sequence_of_comments = skipMany comment -- XXX: we allow empty sequence

-- <comment>::= "*" <any_sequence_of_characters_other_than_EOL> <EOL>
comment :: C e s m => m ()
comment = do
  _ <- char '*' 
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

-- <objective>::= "min:" <zeroOrMoreSpace> <sum> ";"
objective :: C e s m => m Sum
objective = do
  _ <- string "min:"
  zeroOrMoreSpace
  obj <- sum
  semi
  return obj

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
sum = some weightedterm

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
  , char '+' >> unsigned_integer
  , char '-' >> liftM negate unsigned_integer
  ]

-- <unsigned_integer>::= <digit> | <digit><unsigned_integer>
unsigned_integer :: C e s m => m Integer
unsigned_integer = do
  ds <- some digitChar
  return $! readUnsignedInteger ds

-- <relational_operator>::= ">=" | "="
relational_operator :: C e s m => m Op
relational_operator = (string ">=" >> return Ge) <|> (string "=" >> return Eq)

-- <variablename>::= "x" <unsigned_integer>
variablename :: C e s m => m Var
variablename = do
  _ <- char 'x'
  i <- unsigned_integer
  return $! fromIntegral i

-- <oneOrMoreSpace>::= " " [<oneOrMoreSpace>]
oneOrMoreSpace :: C e s m => m ()
oneOrMoreSpace  = skipSome (char ' ')

-- <zeroOrMoreSpace>::= [" " <zeroOrMoreSpace>]
zeroOrMoreSpace :: C e s m => m ()
-- zeroOrMoreSpace = skipMany (char ' ')
zeroOrMoreSpace = space
-- We relax the grammer and allow more type of spacing

semi :: C e s m => m ()
semi = char ';' >> space
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
  mplus (try $ oneOrMoreSpace >> liftM (l:) (oneOrMoreLiterals)) (return [l])
-- Note that we cannot use sepBy1.
-- In "p `sepBy1` q", p should success whenever q success.
-- But it's not the case here.

-- <literal>::= <variablename> | "~"<variablename>
literal :: C e s m => m Lit
literal = variablename <|> (char '~' >> liftM negate variablename)

-- | Parse a OPB format string containing pseudo boolean problem.
#if MIN_VERSION_megaparsec(5,0,0)
parseOPBString :: String -> String -> Either (ParseError Char Dec) Formula
#else
parseOPBString :: String -> String -> Either ParseError Formula
#endif
parseOPBString = parse (formula <* eof)

-- | Parse a OPB format lazy bytestring containing pseudo boolean problem.
#if MIN_VERSION_megaparsec(5,0,0)
parseOPBByteString :: String -> ByteString -> Either (ParseError Char Dec) Formula
#else
parseOPBByteString :: String -> ByteString -> Either ParseError Formula
#endif
parseOPBByteString = parse (formula <* eof)

-- | Parse a OPB file containing pseudo boolean problem.
#if MIN_VERSION_megaparsec(5,0,0)
parseOPBFile :: FilePath -> IO (Either (ParseError Char Dec) Formula)
#else
parseOPBFile :: FilePath -> IO (Either ParseError Formula)
#endif
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
  _ <- char '['
  zeroOrMoreSpace
  cost <- unsigned_integer
  zeroOrMoreSpace
  _ <- char ']'
  zeroOrMoreSpace -- XXX
  c <- constraint
  return (Just cost, c)

-- | Parse a WBO format string containing weighted boolean optimization problem.
#if MIN_VERSION_megaparsec(5,0,0)
parseWBOString :: String -> String -> Either (ParseError Char Dec) SoftFormula
#else
parseWBOString :: String -> String -> Either ParseError SoftFormula
#endif
parseWBOString = parse (softformula <* eof)

-- | Parse a WBO format lazy bytestring containing pseudo boolean problem.
#if MIN_VERSION_megaparsec(5,0,0)
parseWBOByteString :: String -> ByteString -> Either (ParseError Char Dec) SoftFormula
#else
parseWBOByteString :: String -> ByteString -> Either ParseError SoftFormula
#endif
parseWBOByteString = parse (softformula <* eof)

-- | Parse a WBO file containing weighted boolean optimization problem.
#if MIN_VERSION_megaparsec(5,0,0)
parseWBOFile :: FilePath -> IO (Either (ParseError Char Dec) SoftFormula)
#else
parseWBOFile :: FilePath -> IO (Either ParseError SoftFormula)
#endif
parseWBOFile filepath = do
  s <- BL.readFile filepath
  return $! parse (softformula <* eof) filepath s
