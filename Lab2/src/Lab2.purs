module Lab2 where

import Prelude
import Data.List (List(..))
import Data.List (reverse)
import Data.List (length)
import Data.Maybe(Maybe(..))
import Effect (Effect)
import Effect.Console (log)

{-findIndex
Знайдіть перший індекс елементу списку, для якого виконується предикат.
-}

infix 6 Cons as :

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex pred lst = case lst of
    (x:xs) -> case pred x of
        true -> Just(0)
        false -> case findIndex pred xs of 
            Nothing -> Nothing
            Just n -> Just(1 + n)
    _ -> Nothing

predicate :: Int -> Boolean
predicate a = case a of
    -5-> true
    _ -> false

{-findLastIndex
Знайдіть індекс останнього елементу списку, для якого виконується предикат.
-}
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex pred lst = case(findIndex pred (reverse lst)) of
    Just n -> Just((length lst - n - 1))
    Nothing -> Nothing



test :: Effect Unit
test = do 
    log $ show $ findIndex predicate (2:(3:(1:(2:Nil))))
    log $ show $ findLastIndex predicate (2:(3:(1:(4:Nil))))