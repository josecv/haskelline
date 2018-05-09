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
                                   , TestLabel "basicLeftPrompt" basicLeftPrompt
                                   , TestLabel "basicRightPrompt" basicRightPrompt
    ]

resolveCmdResolvesExisting :: Test.HUnit.Test
resolveCmdResolvesExisting = TestCase $ isJust <$> resolveCmd "ghc" >>= assertBool "For resolveCmd ghc"

resolveCmdFailsToResolveNonExisting :: Test.HUnit.Test
resolveCmdFailsToResolveNonExisting = TestCase $ isNothing <$> resolveCmd "ghccccc" >>= assertBool "for resolveCmd with fake cmd"

basicLeftPrompt :: Test.HUnit.Test
basicLeftPrompt = TestCase $ assertEqual "for basicLeftPrompt"
    ("%k%f%b%K{black} %F{blue}foo %K{magenta}%F{black}\xE0B0 %F{red}baz %K{green}%F{magenta}\xE0B0 %F{white}bar %F{blue}\xE0B1 bla %k%F{green}\xE0B0%f ")
    $ buildLeftPrompt [Nothing
                     , Just (Segment "foo" (Just "blue") (Just "black"))
                     , Just (Segment "baz" (Just "red") (Just "magenta"))
                     , Nothing
                     , Just (Segment "bar" (Just "white") (Just "green"))
                     , Just (Segment "bla" (Just "blue") (Just "green"))
    ]

basicRightPrompt :: Test.HUnit.Test
basicRightPrompt = TestCase $ assertEqual "for basicRightPrompt"
    ("%k%f%b%F{black}\xE0B2%K{black} %F{blue}foo %F{magenta}\xE0B2%K{magenta} %F{red}baz %F{green}\xE0B2%K{green} %F{white}bar %F{blue}\xE0B3 bla %k%f%b")
    $ buildRightPrompt [Just (Segment "foo" (Just "blue") (Just "black"))
                      , Nothing
                      , Just (Segment "baz" (Just "red") (Just "magenta"))
                      , Just (Segment "bar" (Just "white") (Just "green"))
                      , Just (Segment "bla" (Just "blue") (Just "green"))
    ]

main :: IO()
main = defaultMain tests
