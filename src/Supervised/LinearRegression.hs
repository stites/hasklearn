-------------------------------------------------------------------------------
-- |
-- Module    :  Supervised.LinearRegression
-- Copyright :  (c) Sam Stites 2017
-- License   :  MIT
-- Maintainer:  sam@stites.io
-- Stability :  experimental
-- Portability: non-portable
--
-- Fits a linear model with coefficients w = (w_1, ..., w_p) to minimize the
-- residual sum of squares between the observed responses in the dataset, and
-- the responses predicted by the linear approximation. Mathematically it solves
-- a problem of the form:
--
--     \underset{w}{min\,} {|| X w - y||_2}^2
--
-- === Ordinary Least Squares Complexity
--
-- This method computes the least squares solution using a singular value
-- decomposition of X. If X is a matrix of size (n, p) this method has a cost of
-- O(n p^2), assuming that n \geq p.
-------------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module Supervised.LinearRegression where

import Prelude
import qualified Data.Monoid as M
import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra.Data as LD


type Labels  = Vector Double
type Weights = Vector Double
type Inputs  = Matrix Double
type Model   = (Weights, Inputs -> Labels)


fit :: Inputs -> Labels -> Model
fit i l = weightedFit w i l
  where
    nfs :: Int
    nfs = cols i

    w :: Weights
    w =  (vector $ replicate nfs 1) / (konst (fromIntegral nfs) nfs)


weightedFit :: Weights -> Inputs -> Labels -> Model
weightedFit _ _xs y = (w, \i -> snocBias i #> w)
  where
    xs :: Matrix Double
    xs = snocBias _xs

    u, s, v :: Matrix Double
    (u, s, v) = svdM $ tr xs <> xs

    xSqInv :: Matrix Double
    xSqInv = (v <> pinv s) <> tr u

    w :: Vector Double
    w = (xSqInv <> tr xs) #> y


svdM :: Matrix Double -> (Matrix Double, Matrix Double, Matrix Double)
svdM = (\(a,b,c) -> (a, diag b, c)) . svd


consBias :: Matrix Double -> Matrix Double
consBias m = LD.fromColumns $ ones (snd $ size m) : LD.toColumns m


snocBias :: Matrix Double -> Matrix Double
snocBias m = LD.fromColumns $ LD.toColumns m M.<> [ones (snd $ size m)]


ones :: Int -> Vector Double
ones = konst 1


predict :: Model -> Inputs -> Labels
predict (_, fn) i = fn i


-- | Returns the coefficient of determination R^2 of the prediction.
scoreR2Regressor :: (truth ~ Labels) => Model -> truth -> Inputs -> Double
scoreR2Regressor (_, fn) truth i = rSquare truth (fn i)


-- | Compute R^2, the coefficient of determination that
-- indicates goodness-of-fit of a regression.
rSquare :: Labels -> Labels -> Double
rSquare truth pred = 1 - numerator / denominator
  where
    weight :: Vector Double
    weight = 1

    numerator :: Double
    numerator = sumElements $ weight * ((truth - pred) ** 2)

    denominator :: Double
    denominator = sumElements $ weight * ((truth - averageTruth) ** 2)

    averageTruth :: Vector Double
    averageTruth = konst ((sumElements truth) / (fromIntegral n)) n
      where
        n :: Int
        n = size truth


sumColumns :: Matrix Double -> Vector Double
sumColumns = vector . fmap sumElements . toColumns


coefficients :: Model -> Weights
coefficients = fst


data Config = Config
  { nIters       :: Integer  -- ^ The number of iterations the algorithm will train weights for.
  , learningRate :: Double   -- ^ Step length for updating weights
  }


defaultConfigs :: Config
defaultConfigs = Config 100 0.001


