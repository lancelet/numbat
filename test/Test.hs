module Main
    ( main
    )
where

import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )

import qualified Numbat.TCP.Segment.Tests       ( tests )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Numbat.TCP.Segment.Tests.tests]
