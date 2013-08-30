{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Nanomsg
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent (threadDelay)
import Control.Applicative ( (<$>) )
import Data.Maybe (catMaybes)

instance Arbitrary ByteString where
    arbitrary = C.pack <$> arbitrary

-- dummy test
prop_reverse :: [Int] -> Bool
prop_reverse xs =
    xs == reverse (reverse xs)

-- test Pub and Sub sockets
prop_PubSub :: Property
prop_PubSub = monadicIO $ do
    msg <- pick arbitrary
    pre $ not (null msg)
    res <- run $ do
        pub <- socket Pub
        _ <- bind pub "inproc://pubsub"
        sub1 <- socket Sub
        _ <- connect sub1 "inproc://pubsub"
        _ <- subscribe sub1 $ C.pack ""
        sub2 <- socket Sub
        _ <- connect sub2 "inproc://pubsub"
        _ <- subscribe sub2 $ C.pack ""
        threadDelay 1000
        r <- mapM (sendMsg pub sub1 sub2) msg
        close pub
        close sub1
        close sub2
        threadDelay 1000
        return r
    assert $ and res
        where
            sendMsg pub sub1 sub2 msg = do
                send pub msg
                send pub msg
                a <- recv sub1
                b <- recv sub1
                c <- recv sub2
                d <- recv sub2
                return $ a == msg && b == msg && c == msg && d == msg

-- test Pair sockets
prop_Pair :: Property
prop_Pair = monadicIO $ do
    msg <- pick arbitrary
    pre $ not (null msg)
    res <- run $ do
        s1 <- socket Pair
        _ <- bind s1 "inproc://pair"
        s2 <- socket Pair
        _ <- connect s2 "inproc://pair"
        threadDelay 1000
        -- Send message from s1 to s2, then back from s2 to s1, then make sure it hasn't changed
        r <- mapM (\m -> send s1 m >> recv s2 >>= send s2 >> recv s1 >>= return . (== m)) msg
        close s1
        close s2
        threadDelay 1000
        return r
    assert $ and res

-- test Pipeline (Push & Pull) sockets
prop_Pipeline :: Property
prop_Pipeline = monadicIO $ do
    msg <- pick arbitrary
    pre $ not (null msg)
    res <- run $ do
        push <- socket Push
        _ <- bind push "inproc://pipeline"
        pull1 <- socket Pull
        pull2 <- socket Pull
        _ <- connect pull1 "inproc://pipeline"
        _ <- connect pull2 "inproc://pipeline"
        threadDelay 1000
        r <- mapM (testSockets push pull1 pull2) msg
        close push
        close pull1
        close pull2
        threadDelay 1000
        return r
    assert $ and res
        where
            testSockets push pull1 pull2 msg = do
                send push msg
                send push msg
                send push msg
                threadDelay 1000
                a <- recv' pull1
                b <- recv' pull1
                c <- recv' pull1
                d <- recv' pull2
                e <- recv' pull2
                f <- recv' pull2
                return $ all (== msg) (catMaybes [a, b, c, d, e, f])

main :: IO ()
main = $defaultMainGenerator
