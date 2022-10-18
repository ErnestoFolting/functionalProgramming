module Lab2 where

import Prelude
import Data.List (List(..))
import Data.List (reverse)
import Data.List (length)
import Data.Maybe(Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Data.Tuple (Tuple(..))
import Data.Tuple (fst)
import Data.Tuple (snd)

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

{-zip
zip візьме 2 списки та заархівує їх в один список, де кожен елемент вихідного списку є Tuple який містить перший елемент List у першій позиції та другий List у другій позиції
-}
zip :: forall a b. List a -> List b -> List(Tuple a b)
zip lst1 lst2 = 
    let 
        go :: forall a b. List a -> List b -> List(Tuple a b) -> List(Tuple a b)
        go lst1 lst2 acc = case lst1 of
            (x:xs)->
                case lst2 of
                    (y:ys) ->  go xs ys ((Tuple x y) : acc)
                    _ -> acc
            _ -> acc
    in 
         (go lst1 lst2 Nil)
{-unzip 
unzip є зворотним до zip. Ви повинні мати можливість взяти результат zip і використати його, щоб розархівувати та отримати ваші вихідні списки назад, припускаючи, що ви почали зі списків однакового розміру.
-} 
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip lst = 
    let 
        go :: forall a b. List (Tuple a b) -> List a -> List b -> Tuple (List a) (List b)
        go lst acc1 acc2 = case lst of 
            (x:xs) -> case fst x of
                val1 -> case snd x of
                    val2 -> go xs (val1:acc1) (val2:acc2)
            _ -> Tuple acc1 acc2 
    in
        (go lst Nil Nil)

test :: Effect Unit
test = do 
    log $ show $ findIndex predicate (2:(3:(1:(2:Nil))))
    log $ show $ findLastIndex predicate (2:(3:(1:(4:Nil))))   
    log $ show $ zip (2:(3:(1:(2:Nil)))) (5:(2:(3:(1:(2:Nil)))))
    log $ show $ unzip(zip (2:(3:(1:(2:Nil)))) (5:(2:(3:(1:(2:Nil))))))
    