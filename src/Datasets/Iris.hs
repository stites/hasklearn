--------------------------------------------------------------------------------
-- |
-- Module    :  Datasets.Iris
-- Copyright :  (c) Sam Stites 2017
-- License   :  BSD3
-- Maintainer:  sam@stites.io
-- Stability :  experimental
-- Portability: non-portable
--
-- === Iris Data Set
--
-- Data Set Characteristics  :  Multivariate
-- Number of Instances       : 150
-- Area                      : Life
-- Attribute Characteristics : Real
-- Number of Attributes      : 4
-- Date Donated              : 1988-07-01
-- Associated Tasks          : Classification
-- Missing Values?           : No
-- Creator                   : R. A. Fisher
-- Source                    : https://archive.ics.uci.edu/ml/datasets/Iris
--
-- === Data Set Information
--
-- This is perhaps the best known database to be found in the pattern
-- recognition literature. Fisher's paper is a classic in the field and is
-- referenced frequently to this day. (See Duda & Hart, for example.) The data
-- set contains 3 classes of 50 instances each, where each class refers to a
-- type of iris plant. One class is linearly separable from the other 2; the
-- latter are NOT linearly separable from each other.
--
-- Predicted attribute: class of iris plant.
--
-- This is an exceedingly simple domain.
--
-- This data differs from the data presented in Fishers article (identified by
-- Steve Chadwick, spchadwick '@' espeedaz.net ). The 35th sample should be:
-- 4.9,3.1,1.5,0.2,"Iris-setosa" where the error is in the fourth feature. The
-- 38th sample: 4.9,3.6,1.4,0.1,"Iris-setosa" where the errors are in the second
-- and third features.
--
--
-- === Attribute Information
--
-- 1. Sepal length in cm
-- 2. Sepal width in cm
-- 3. Petal length in cm
-- 4. Petal width in cm
-- 5. Class: Iris Setosa | Iris Versicolour | Iris Virginica
--
-- === Summary Statistics:
--
--               Min  Max   Mean    SD   Class Correlation
-- sepal length: 4.3  7.9   5.84  0.83    0.7826
-- sepal width : 2.0  4.4   3.05  0.43   -0.4194
-- petal length: 1.0  6.9   3.76  1.76    0.9490  (high!)
-- petal width : 0.1  2.5   1.20  0.76    0.9565  (high!)
--
-- === Class Distribution
--
-- 33.3% for each of 3 classes.
--------------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
module Datasets.Iris where

import Prelude hiding (readFile)
import Data.Csv
import Control.Arrow
import GHC.Generics
import Data.ByteString.Lazy (readFile)
import Control.Monad.IO.Class
import Control.Exception.Safe

import Data.Vector (Vector, toList, fromList)
import qualified Data.Vector as V
import Numeric.LinearAlgebra (Matrix, fromLists)
import Numeric.LinearAlgebra.Data ((??), Extractor(..))

data Datum = Datum
  { sepalLength :: Double
  , sepalWidth  :: Double
  , petalLength :: Double
  , petalWidth  :: Double
  , irisClass   :: IrisClass
  } deriving (Eq, Show, Generic)

instance FromRecord Datum

toDoubles :: Datum -> [Double]
toDoubles (Datum a b c d e) = [a, b, c, d, fromIntegral $ fromEnum e]
-- ========================================================================= --

data IrisClass
  = Setosa
  | Versicolour
  | Virginica
  deriving (Eq, Bounded, Enum, Generic)

instance FromRecord IrisClass
instance FromField  IrisClass where
  parseField s
    | s == "Iris-setosa"     = pure Setosa
    | s == "Iris-versicolor" = pure Versicolour
    | s == "Iris-virginica"  = pure Virginica

instance Show IrisClass where
  show Setosa      = "Iris Setosa"
  show Versicolour = "Iris Versicolour"
  show Virginica   = "Iris Virginica"

-- ========================================================================= --

loadIrisRaw :: (MonadThrow m, MonadIO m) => m (Vector Datum)
loadIrisRaw = do
  iris <- liftIO $ readFile "data/Datasets/Iris.csv"
  Right xs <- pure $ decode NoHeader iris
  return xs


loadIris :: (MonadThrow m, MonadIO m) => m (Matrix Double, Vector IrisClass)
loadIris = featureLabelSplit <$> loadIrisRaw
  where
    featureLabelSplit :: Vector Datum -> (Matrix Double, Vector IrisClass)
    featureLabelSplit v = foo $ fmap ((init . toDoubles) &&& irisClass) v

    foo :: Vector ([Double], IrisClass) -> (Matrix Double, Vector IrisClass)
    foo vs = (fromLists $ toList (fmap fst vs), fmap snd vs)


type Dataset = (Matrix Double, Vector IrisClass)


splitCV :: Float -> Dataset -> (Dataset, Dataset)
splitCV percent (features, labels) =
  ( (features ?? (Take splitpoint, All), V.take splitpoint labels)
  , (features ?? (Drop splitpoint, All), V.drop splitpoint labels)
  )

  where
    splitpoint :: Int
    splitpoint = truncate (fromIntegral (length labels) * percent) :: Int

