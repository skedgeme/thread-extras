{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent.Thread.BoundedThreadGroup
import Control.Monad (forM)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Word (Word)
import Test.Hspec (Spec, describe, hspec, it, shouldSatisfy)
import Test.Hspec.QuickCheck (modifyMaxSize)
import Test.QuickCheck (property)

doWork :: Monad m => Word -> m ()
doWork n = seq (fib n) (return ())

fib :: Word -> Word
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = hspec spec


spec :: Spec
spec =
  describe "bounded thread groups" $ modifyMaxSize (const 1000) $ do
    it "will only run n threads" $ property $
      \(n'::Int,work::[Word]) -> do
        running <- newIORef 0
        let n = abs n'
        tg <- new n
        threads <- forM (take (10*n) work)
          (\w -> forkIO tg $ do
              now <- atomicModifyIORef' running (\x -> (x+1,x+1))
              doWork (abs w `mod` 20)
              final <- atomicModifyIORef' running (\x -> (x-1,x-1))
              return (now <= n && final >=0)
          )
        results <- mapM snd threads
        results `shouldSatisfy` all (\x -> case x of Right True -> True; _ -> False)
