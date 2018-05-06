module Main where

import Test.HUnit

import Hline.Cmd
import Hline.Prompt
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Maybe

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList [TestLabel "resolveExisting" resolveCmdResolvesExisting
                                   , TestLabel "resolveNonExisting" resolveCmdFailsToResolveNonExisting
                                   , TestLabel "buildLeftPromptTest" basicLeftPrompt]

resolveCmdResolvesExisting :: Test.HUnit.Test
resolveCmdResolvesExisting = TestCase $ isJust <$> resolveCmd "ghc" >>= assertBool "For resolveCmd ghc"

resolveCmdFailsToResolveNonExisting :: Test.HUnit.Test
resolveCmdFailsToResolveNonExisting = TestCase $ isNothing <$> resolveCmd "ghccccc" >>= assertBool "for resolveCmd with fake cmd"

basicLeftPrompt :: Test.HUnit.Test
basicLeftPrompt = TestCase $ assertEqual "for basicLeftPrompt"
    ("%k%f%b%K{black} %F{blue}foo %K{magenta}%F{black}\xE0B0 %F{red}baz %K{green}%F{magenta}\xE0B0 %F{white}bar %k%F{green}\xE0B0%f ")
    $ buildLeftPrompt [Nothing
                  , Just (Segment "foo" (Just "blue") (Just "black"))
                  , Just (Segment "baz" (Just "red") (Just "magenta"))
                  , Nothing
                  , Just (Segment "bar" (Just "white") (Just "green"))]

main :: IO()
main = defaultMain tests
