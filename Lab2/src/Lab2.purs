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
    val | val > 3 -> true
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
         reverse (go lst1 lst2 Nil)
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
        go (reverse lst) Nil Nil

{-filter
filter takes a Predicate Function to KEEP or filter-in elements when the Predicate returns true.
-} 
filter :: forall a. (a -> Boolean) -> List a -> List a
filter pred lst = case lst of
    (x:xs) -> case pred x of
        true -> x : (filter pred xs)
        false -> filter pred xs 
    _ -> Nil                     

{-tail recursion filter
Оптимізуйте реалізацію фільтра використовуючи підхід хвостової оптимізації
-} 
tailFilter :: forall a. (a -> Boolean) -> List a -> List a
tailFilter pred lst = 
    let 
        go :: forall a. (a -> Boolean) -> List a ->List a -> List a
        go pred lst acc = case lst of
            (x:xs) -> case pred x of
                true -> go pred xs (x:acc)
                false -> go pred xs acc
            _ -> acc
    in
        reverse (go pred lst Nil)

{-take
take поверне вказану кількість елементів зі списку або стільки, скільки зможе, якщо список замалий.
-} 
take :: forall a. Int -> List a -> List a
take number lst = case lst of
    (x:xs) -> case number of
        val| val > 0 -> x : take (number-1) xs
        _ -> Nil
    _ -> Nil

{-tail recursion take 
Оптимізуйте реалізацію take використовуючи підхід хвостової оптимізації
-} 

tailTake :: forall a. Int -> List a -> List a
tailTake number lst = 
    let 
        go :: forall a. Int -> List a -> List a -> List a
        go number lst acc = case lst of
            (x:xs) -> case number of
                val| val > 0 ->
                    go (number-1) xs (x:acc)
                _ -> acc
            _ -> acc
    in reverse(go number lst Nil)

test :: Effect Unit
test = do 
    log $ show $ findIndex predicate (2:(3:(8:(2:Nil))))
    log $ show $ findLastIndex predicate (2:(3:(1:(4:Nil))))   
    log $ show $ zip (2:(3:(1:(2:Nil)))) (5:(2:(3:(1:(2:Nil)))))
    log $ show $ unzip(zip (2:(3:(1:(2:Nil)))) (5:(2:(3:(1:(2:Nil))))))
    log $ show $ filter predicate (7:(3:(1:(4:Nil))))
    log $ show $ tailFilter predicate (7:(3:(1:(4:Nil))))
    log $ show $ take 8 (7:(3:(1:(4:Nil))))
    log $ show $ tailTake 2 (7:(3:(1:(4:Nil))))
    