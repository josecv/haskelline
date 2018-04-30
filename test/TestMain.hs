module Main where

import Test.HUnit

import Hline.Cmd
import Data.Maybe

tests :: Test
tests = TestList [TestLabel "resolveExisting" resolveCmdResolvesExisting, TestLabel "resolveNonExisting" resolveCmdFailsToResolveNonExisting]

resolveCmdResolvesExisting :: Test
resolveCmdResolvesExisting = TestCase $ isJust <$> resolveCmd "ghc" >>= assertBool "For resolveCmd ghc"

resolveCmdFailsToResolveNonExisting :: Test
resolveCmdFailsToResolveNonExisting = TestCase $ isNothing <$> resolveCmd "ghccccc" >>= assertBool "for resolveCmd with fake cmd"

main :: IO Counts
main = runTestTT tests
