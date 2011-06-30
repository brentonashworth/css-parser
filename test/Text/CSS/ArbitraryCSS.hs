{-# LANGUAGE FlexibleInstances #-}

module Text.CSS.ArbitraryCSS where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.List (intersperse)
import Text.Regex
import Text.Regex.Posix

data CSSString a = CSSString a String
data NewLine = NewLine
-- | A string of one or more space characters.
data Spaces = Spaces
-- | A string of 0 or 1 Spaces.
data WhiteSpace = WhiteSpace
-- | A number like 42, 4.2 or .42
data Number = Number
-- | A comment is any character sequence which start with /* and ends with */
data Comment = Comment
-- | A unicode digit begins with \\ and then contains up to 6 hex
-- digits. If there are less that 6 hex digits then a whitespace
-- character must be added to the end.
data Unicode = Unicode
-- | A nonascii character is a character in the range \o240 - \o4177777
data Nonascii = Nonascii
-- | An escape is either a unicode character or a backslash followed
-- | by anthing but \r, \n, \f or a hex digit.
data Escape = Escape
-- | A name character is a digit, letter, underscore, dash, nonascii
-- | or escape.
data NameChar = NameChar

digitChar = ['0'..'9']
hexChar = digitChar ++ ['a'..'f'] ++ ['A'..'F']
letterChar = ['a'..'z'] ++ ['A'..'Z']
spaceChar = " \t\r\n\f"
anyChar = letterChar ++ digitChar ++ spaceChar ++ 
          "~`!@#$%^&*()_-+={[}]|\\:;\"'<,>.?/"
newLineChar = [ "\r\n", "\n", "\r", "\f"]
escapeChar = filter (`notElem` ("\r\n\f" ++ hexChar)) anyChar

digits :: Gen String
digits = listOf1 $ elements digitChar

hexDigits :: Gen String
hexDigits = listOf1 $ elements hexChar

instance Show (CSSString a) where 
  show (CSSString _ x) = x

instance Arbitrary (CSSString NewLine) where
  arbitrary = do x <- elements newLineChar
                 return $ CSSString NewLine x

instance Arbitrary (CSSString Spaces) where
  arbitrary = do x <- listOf1 $ elements spaceChar
                 return $ CSSString Spaces x

-- | Note the difference between this and the above is that here you
-- may have an empty string.
instance Arbitrary (CSSString WhiteSpace) where
  arbitrary = do x <- listOf $ elements spaceChar
                 return $ CSSString WhiteSpace x

instance Arbitrary (CSSString Number) where
  arbitrary = do n <- choose (1,3) :: Gen Int
                 case n of
                      1 -> do x <- digits
                              return $ CSSString Number x
                      2 -> do a <- digits
                              b <- digits
                              return $ CSSString Number $ a ++ "." ++ b
                      3 -> do a <- digits
                              return $ CSSString Number $ "." ++ a

instance Arbitrary (CSSString Comment) where
  arbitrary = do x <- listOf $ elements anyChar
                 let c = concat $ splitRegex (mkRegex "[*]/") x
                 return $ CSSString Comment $ ("/*" ++ c ++ "*/")

instance Arbitrary (CSSString Unicode) where
  arbitrary = do x <- hexDigits
                 let x' = take 6 x
                 if (length x') < 6
                 then do s <- elements (newLineChar ++ [" "])
                         return $ CSSString Unicode ("\\" ++ x' ++ s)
                 else return $ CSSString Unicode ("\\" ++ x')

instance Arbitrary (CSSString Nonascii) where
  arbitrary = do x <- elements ['\o240'..'\o4177777']
                 return $ CSSString Nonascii [x]

instance Arbitrary (CSSString Escape) where
  arbitrary = do n <- choose (1,2) :: Gen Int
                 case n of
                      1 -> do x <- arbitrary :: Gen (CSSString Unicode)
                              return $ CSSString Escape $ show x
                      2 -> do x <- elements escapeChar
                              return $ CSSString Escape ("\\" ++ [x])

instance Arbitrary (CSSString NameChar) where
  arbitrary = do n <- choose (1,3) :: Gen Int
                 case n of
                      1 -> do x <- elements (letterChar ++ "_-")
                              return $ CSSString NameChar [x]
                      2 -> do x <- arbitrary :: Gen (CSSString Nonascii)
                              return $ CSSString NameChar $ show x
                      3 -> do x <- arbitrary :: Gen (CSSString Unicode)
                              return $ CSSString NameChar $ show x
