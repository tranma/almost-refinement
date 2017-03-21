{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Liar where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Monadic


-- A tells you whether a number is even or odd.
-- A is a good abstract specification, since there are semantic assertions you can make about A.
-- e.g. A(n) == True ==> A(n+1) = False
-- A can also be used as a concrete implementation. What could be an abstract?
--
a :: Int -> Bool
a x = x `rem` 2 == 0

-- B might lie about the result.
-- B does not refine A, because B might make the non-deterministic choice of whether to lie about the
-- result. This is a non-determinisitc choice because it's not/can't be included in the model.
-- B can be used as an abstract specification, but it's rather useless. What can you say about B?
--
b :: Int -> IO Bool
b x = do
  let result = x `rem` 2 == 0
  putStrLn "Should I lie? [y/n]"
  c <- getChar
  putStrLn ""
  case c of
    'y' -> return (not result)
    _ -> return result

--------------------------------------------------------------------------------

-- Some correctness properties about A
prop_a :: Int -> Property
prop_a x = property (a x /= a (x + 1))

-- B does not refine A
prop_b_a :: Int -> Property
prop_b_a x = monadicIO $ do
  let a' = a x
  b' <- run $ b x
  stop $ a' === b'

-- A does refine B, given the right non-deterministic choice ('y')
-- because B has strictly more transitions and states than A.

not_refine :: IO ()
not_refine = quickCheckWith (stdArgs { maxSuccess = 3 }) prop_b_a
