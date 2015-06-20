{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PseudoBoolean.ByteStringBuilder
-- Copyright   :  (c) Masahiro Sakai 2011-2015
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.PseudoBoolean.ByteStringBuilder
  (
  -- * Builder for (Lazy) ByteString generation
    opbBuilder
  , wboBuilder

  -- * Lazy ByteString generation
  , toOPBByteString
  , toWBOByteString

  -- * File I/O
  , writeOPBFile
  , writeWBOFile
  , hPutOPB
  , hPutWBO
  ) where

import qualified Prelude
import Prelude hiding (sum)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Monoid hiding (Sum (..))
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Builder (Builder, intDec, integerDec, char7, string7, hPutBuilder, toLazyByteString)
import System.IO
import Data.PseudoBoolean.Types

-- | A ByteString Builder which renders a OPB format byte-string containing pseudo boolean problem.
opbBuilder :: Formula -> Builder
opbBuilder opb = (size <> part1 <> part2)
  where
    nv = pbNumVars opb
    nc = pbNumConstraints opb
    p = pbProducts opb
    np = Set.size p
    sp = Prelude.sum [IntSet.size tm | tm <- Set.toList p]
    size = string7 "* #variable= " <> intDec nv <> string7 " #constraint= " <> intDec nc
         <> (if np >= 1 then string7 " #product= " <> intDec np <> string7 " sizeproduct= " <> intDec sp else mempty)
         <> char7 '\n'
    part1 = 
      case pbObjectiveFunction opb of
        Nothing -> mempty
        Just o -> string7 "min: " <> showSum o <> string7 ";\n"
    part2 = mconcat $ map showConstraint (pbConstraints opb)

-- | A ByteString Builder which renders a WBO format byte-string containing weighted boolean optimization problem.
wboBuilder :: SoftFormula -> Builder
wboBuilder wbo = size <> part1 <> part2
  where
    nv = wboNumVars wbo
    nc = wboNumConstraints wbo
    p = wboProducts wbo
    np = Set.size p
    sp = Prelude.sum [IntSet.size tm | tm <- Set.toList p]
    size = string7 "* #variable= " <> intDec nv <> string7 " #constraint= " <> intDec nc
         <> (if np >= 1 then string7 " #product= " <> intDec np <> string7 " sizeproduct= " <> intDec sp else mempty)
         <> char7 '\n'
    part1 = 
      case wboTopCost wbo of
        Nothing -> string7 "soft: ;\n"
        Just t -> string7 "soft: " <> integerDec t <> string7 ";\n"
    part2 = mconcat $ map showSoftConstraint (wboConstraints wbo)

showSum :: Sum -> Builder
showSum = mconcat . map showWeightedTerm

showWeightedTerm :: WeightedTerm -> Builder
showWeightedTerm (c, lits) = foldr (\f g -> f <> char7 ' ' <> g) mempty (x:xs)
  where
    x = if c >= 0 then char7 '+' <> integerDec c else integerDec c
    xs = map showLit lits

showLit :: Lit -> Builder
showLit lit = if lit > 0 then v else char7 '~' <> v
  where
    v = char7 'x' <> intDec (abs lit)

showConstraint :: Constraint -> Builder
showConstraint (lhs, op, rhs) =
  showSum lhs <> f op <>  char7 ' ' <> integerDec rhs <> string7 ";\n"
  where
    f Eq = char7 '='
    f Ge = string7 ">="

showSoftConstraint :: SoftConstraint -> Builder
showSoftConstraint (cost, constr) =
  case cost of
    Nothing -> showConstraint constr
    Just c -> char7 '[' <> integerDec c <> string7 "] " <> showConstraint constr



-- | Generate a OPB format byte-string containing pseudo boolean problem.
toOPBByteString :: Formula -> BS.ByteString
toOPBByteString opb = toLazyByteString (opbBuilder opb)

-- | Generate a WBO format byte-string containing weighted boolean optimization problem.
toWBOByteString :: SoftFormula -> BS.ByteString
toWBOByteString wbo = toLazyByteString (wboBuilder wbo)

-- | Output a OPB file containing pseudo boolean problem.
writeOPBFile :: FilePath -> Formula -> IO ()
writeOPBFile filepath opb = withBinaryFile filepath WriteMode $ \h -> do
  hSetBuffering h (BlockBuffering Nothing)
  hPutOPB h opb

-- | Output a WBO file containing weighted boolean optimization problem.
writeWBOFile :: FilePath -> SoftFormula -> IO ()
writeWBOFile filepath wbo = withBinaryFile filepath WriteMode $ \h -> do
  hSetBuffering h (BlockBuffering Nothing)
  hPutWBO h wbo

-- | Output a OPB file to a 'Handle' using 'hPutBuilder'.
--
-- It is recommended that the 'Handle' is set to binary and
-- 'BlockBuffering' mode. See 'hSetBinaryMode' and 'hSetBuffering'.
--
-- This function is more efficient than 'hPut' . 'toOPBByteString'
-- because in many cases no buffer allocation has to be done.
hPutOPB :: Handle -> Formula -> IO ()
hPutOPB h opb = hPutBuilder h (opbBuilder opb)


-- | Output a WBO file to a 'Handle' using 'hPutBuilder'.
--
-- It is recommended that the 'Handle' is set to binary and
-- 'BlockBuffering' mode. See 'hSetBinaryMode' and 'hSetBuffering'.
--
-- This function is more efficient than 'hPut' . 'toWBOByteString'
-- because in many cases no buffer allocation has to be done.
hPutWBO :: Handle -> SoftFormula -> IO ()
hPutWBO h wbo = hPutBuilder h (wboBuilder wbo)
