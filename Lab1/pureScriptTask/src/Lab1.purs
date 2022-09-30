module Lab1
  where


import Prelude
import Data.List (List(..))
import Data.Maybe(Maybe(..))
import Effect (Effect)
import Effect.Console (log)

{-Створити функцію Singleton, яка приймає на вхід один аргумент і повертає список із одного елемента (аргумента функції). 
Створити функцію null, яка приймає на вхід список і повертає boolean з результатом перевірки чи пустий список.
Створити функцію snoc, яка приймає на вхід список і елемент, і повертає новий список із елементом, який доданий в кінець списку.
Створити функцію length, яка приймає на вхід список і повертає кількість елементів в списку.
-}


infix 6 Cons as :

singleton :: forall a. a -> List a
singleton a = a : Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc lst needToAdd = case lst of
  Nil -> needToAdd : Nil
  x : xs -> (x : (snoc xs needToAdd))

length :: forall a. List a -> Int
length lst = case lst of
  Nil -> 0
  x : xs -> 1 + length xs

test :: Effect Unit
test = do
  log $ show $ singleton 5
  log $ show $ null (1 : (2 : Nil))
  log $ show $ snoc (1 : (2 : (3 : Nil))) 4
  log $ show $ length (1 : (2 : Nil))