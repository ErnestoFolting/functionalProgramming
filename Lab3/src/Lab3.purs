module Lab3 where

import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (>=), (<>))
import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)
import Effect (Effect)
import Effect.Console (log)

data Maybe a
  = Nothing
  | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false

instance ordMaybe :: Ord a => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare (Just x) (Just y) = compare x y
  compare (Just _) (Nothing) = GT
  compare (Nothing) (Just _) = LT

instance showMaybe :: Show a => Show (Maybe a) where
  show (Just a) = "(Just " <> show a <> ")."
  show Nothing = "Nothing."

test :: Effect Unit
test = do
  log $ show $ "Example"
  log $ show $ Just 5 == Just 5 
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log $ show $ "Subtask1 and subtask2"
  log "------------------"
  log $ show $ Just 1 < Just 5 
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log $ show $ "Subtask3"
  log $ show $ Just "abc"
  log $ show $ (Nothing :: Maybe Unit)
