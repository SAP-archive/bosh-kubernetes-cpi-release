module Test where

import Control.Monad
import Control.Applicative

monadic1 :: IO a
monadic1 = undefined

monadic2 :: a -> IO a
monadic2 = undefined

f :: a -> a -> IO a
f = undefined

main :: IO ()
main = do
  (\_ -> monadic1) >=> monadic2 ()
  return ()
