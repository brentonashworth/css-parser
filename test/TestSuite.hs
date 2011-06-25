module TestSuite where

import Test.Framework (defaultMain, testGroup)

import qualified Text.CSS.CSSParserTests

main :: IO ()
main = defaultMain
    [ testGroup "CSS Parser" Text.CSS.CSSParserTests.tests
    ]
