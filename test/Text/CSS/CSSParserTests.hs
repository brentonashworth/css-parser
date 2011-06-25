module Text.CSS.CSSParserTests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test, State)
import Data.Monoid
import Text.ParserCombinators.Parsec

import Text.CSS.CSSParser
import Text.CSS.ArbitraryCSS

options :: TestOptions
options = mempty {topt_maximum_generated_tests = Just 1000}

tests :: [Test]
tests = map (plusTestOptions options)
        [ testProperty "new line" prop_new_line
        , testProperty "fail new line" prop_fail_new_line
        , testProperty "spaces" prop_spaces
        , testProperty "fail spaces" prop_fail_spaces
        , testProperty "whitespace" prop_whitespace
        , testProperty "number" prop_number
        , testProperty "fail number" prop_fail_number
        , testProperty "comment" prop_comment
        , testProperty "fail comment" prop_fail_comment
        , testProperty "unicode" prop_unicode
        , testProperty "fail unicode" prop_fail_unicode
        , testProperty "nonascii" prop_nonascii
        , testProperty "fail nonascii" prop_fail_nonascii
        ]

-- | Return True is parsing is successful.
canParse :: Show a => Parser String -> a -> Bool
canParse p x = 
  let xString = show x in
  case run p xString of
    Right _ -> True
    Left _  -> False

-- | Return True if parsing fails.
failParse :: Show a => Parser String -> a -> Bool
failParse p x = 
  let xString = show x in
  case run p xString of
    Right _ -> False
    Left _  -> True

trim :: String -> String
trim = reverse . dls . reverse . dls
       where space x = x `notElem` " \t\r\n\f"
             dls = dropWhile (not . space)

-- | Return True if the parsed data is the same as the result of
-- applying f to the input string.
canParseF :: Show a => (String -> String) -> Parser String -> a -> Bool
canParseF f p x = 
  let xString = show x in
  case run p xString of
    Right result -> result == f xString
    Left _  -> False

-- | Return True is the parser completely consumes the input.
canParseExact :: Show a => Parser String -> a -> Bool
canParseExact p x = 
  let xString = show x in
  case run p xString of
    Right result -> result == xString
    Left _  -> False

prop_new_line x = canParseExact nl x
  where types = [x :: CSSString NewLine]

prop_spaces x = canParseExact s x
  where types = [x :: CSSString Spaces]

prop_whitespace x = canParseExact w x
  where types = [x :: CSSString WhiteSpace]

prop_number x = canParseExact num x
  where types = [x :: CSSString Number]

prop_comment x = canParseExact comment x
  where types = [x :: CSSString Comment]

prop_unicode x = canParseF trim unicode x
  where types = [x :: CSSString Unicode]

prop_nonascii x = canParse nonascii x 
  where types = [x :: CSSString Nonascii]

-- | As a sanity check, ensure that we aren't just parsing
-- | anything. It would be better we could randomly generate any kind
-- | of data which does not include a particular type.

prop_fail_new_line x = failParse nl x
  where types = [x :: CSSString Number]

prop_fail_spaces x = failParse s x
  where types = [x :: CSSString Number]

prop_fail_number x = failParse num x
  where types = [x :: CSSString Comment]

prop_fail_comment x = failParse comment x
  where types = [x :: CSSString Number]

prop_fail_unicode x = failParse unicode x
  where types = [x :: CSSString Nonascii]

prop_fail_nonascii x = failParse nonascii x 
  where types = [x :: CSSString Number]
