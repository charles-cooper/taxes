module Main where
import Control.Monad
import Data.Map
import Data.Monoid
import Data.Function
import Debug.Trace

-- single filers
irsBrackets = [ (0     , 0.10)
              , (9275  , 0.15)
              , (37650 , 0.25)
              , (91150 , 0.28)
              , (190150, 0.33)
              , (413350, 0.35)
              , (415050, 0.396)
              ]

taxOn :: Double -> Double
taxOn income = let
  cutoffs = fst <$> irsBrackets
  rates   = snd <$> irsBrackets
  in scanl1 (-) cutoffs
    & fmap (+income)
    & zipWith min (drop 1 cutoffs)
    & takeWhile (>0)
    -- & (\x -> trace (show x) x)
    & zipWith (*) rates
    & sum

prettyDollars :: Double -> String
prettyDollars = ("$"<>) . show . (/100) . (fromInteger . round) . (*100)

main :: IO ()
main = forever $ do
  putStrLn "Enter hypothetical income:"
  income <- read <$> getLine
  putStrLn $ "Hypothetical taxes on "
    <> prettyDollars income
    <> " would be "
    <> prettyDollars (taxOn income)
    <> "."

