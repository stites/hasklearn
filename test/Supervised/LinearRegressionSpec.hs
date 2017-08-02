module Supervised.LinearRegressionSpec where

import Prelude
import Test.Hspec
import Supervised.LinearRegression
import Datasets.Iris
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA

v2la :: V.Vector IrisClass -> LA.Vector Double
v2la = LA.vector . V.toList . fmap (fromIntegral . fromEnum)

main :: IO ()
main = do
  ((trainX, trainY'), (testX, testY')) <- splitCV 0.6 <$> loadIris
  let
    !trainY = v2la trainY' :: LA.Vector Double
    !testY  = v2la testY' :: LA.Vector Double

  let
    trainedModel :: Model
    trainedModel = fit mkLinearRegression trainX trainY

  let
    predicted :: Labels
    predicted = trainedModel `predict` testX

  let
    score :: Double
    score = rSquare testY predicted

  print $ score
--  hspec spec

spec :: Spec
spec =
  describe "Linear Regression" $ undefined


