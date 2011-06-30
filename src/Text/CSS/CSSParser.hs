
-- | A CSS 2.1 Parser. 
--
-- See <http://www.w3.org/TR/CSS21/syndata.html> for a description of
-- CSS 2.1 syntax, grammer and data types. This parser follows the
-- more restrictive grammer found in
-- <http://www.w3.org/TR/CSS21/grammar.html>. Many of the names below
-- come directly from this document. Most parsers are a direct
-- translation of the specification. Higher level parsers may differ a
-- bit in order to account for additional CSS semantics not expressed
-- in the grammer. Any parts of this specification which are not
-- implemented will be listed here. They are left as an excersice for
-- the reader. :)
--

module Text.CSS.CSSParser
    ( -- * Compound Parsers
      nmchar
    , escape
      -- * Simple Parsers
    , nonascii
    , unicode
    , comment
    , num
    , w
    , s
    , nl
      -- * Run
    , run
    ) where

import Text.ParserCombinators.Parsec

-- | Parse a name character.
--
-- > [_a-z0-9-]|{nonascii}|{escape}
--
nmchar :: Parser String
nmchar = (do x <- (letter <|> digit <|> oneOf "_-")
             return [x]) <|> (try nonascii) <|> escape <?> "nmchar"

hexDigits = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'] 

-- | Parse an escape character.
--
-- > {unicode}|\\[^\r\n\f0-9a-f]
--
-- An escape character is either a unicode character or a backslash
-- followed by any character other than \\r, \\n, \\f or a hexadecimal
-- digit.
escape :: Parser String
escape = (try unicode) <|> (do a <- char '\\'
                               b <- noneOf ("\n\r\f"++hexDigits)
                                    <?> "escaped character"
                               return $ (a:[b])) <?> "escape"

-- | Parse a nonascii character.
-- 
-- > [\240-\377]
--
-- A nonascii character is a character in the octal range 240-377. The
-- specification uses the upper limit of 377 because that is the
-- highest character number that current version of Flex can deal
-- with. The actual upper limit is \\4177777 (decimal 1114111 and hex
-- \\x10ffff) which is the highest possible code point in
-- Unicode/ISO-10646. This parser is implemented to use the correct
-- upper bound.
nonascii :: Parser String
nonascii = do x <- anyChar
              if x >= '\o240' && x <= '\o4177777'
                then return [x]
                else unexpected "ascii character"

-- Question: Is there a way I can make the above error message show up
-- in the expecting clause of the output error message?

-- | Find the end of a unicode character treating \\r\\n as a single
-- white space character.
unicodeEnd :: Parser String
unicodeEnd =   try (string "\r\n") 
           <|> string " "  
           <|> string "\n" 
           <|> string "\r" 
           <|> string "\t" 
           <|> string "\f" <?> "unicode end"

-- | Parse a unicode character.
--
-- > \\[0-9a-f]{1,6}(\r\n|[ \t\r\n\f])?
--
-- A unicode character starts with a backslash and then contains up to
-- six hex digits. If a character in the range [0-9a-fA-F] follows the
-- hexadecimal number, the end of the number needs to be made
-- clear. This is done by providing exactly 6 digits or by adding a
-- single white space character at the end.
--
-- This parser will grab any valid hexadecimal number. The following
-- restriction is not implemented in this parser.
--
-- If the number is outside the range allowed by Unicode (e.g.,
-- "\110000" is above the maximum 10FFFF allowed in current Unicode),
-- the UA may replace the escape with the "replacement character"
-- (U+FFFD). 
--
-- This should implemented at a level above the parser.
-- 
-- See
-- <http://www.w3.org/International/questions/qa-escapes#cssescapes>
-- for another example of how these are used.
unicode :: Parser String
unicode = (do a <- string "\\"
              b <- try (count 6 hexDigit) 
                 <|> (do c <- many1 hexDigit
                         unicodeEnd
                         return c) <?> "unicode digits"
              return $ a ++ b) <?> "unicode"

-- | Parse a comment.
--
-- > \/\* [^*]*\*+ ([^/*][^*]*\*+)* \/
--
-- A comment starts with \/* and ends with *\/ and may contain
-- anything except for the sequence \"*\/\" (which would end the
-- comment early and leave a stranded comment tail). No nested
-- comments. Parsing will fail if there are nested comments.
--
-- Comments may appear anywhere outside of other tokens. This means
-- that a \/* ... *\/ sequence may appear within a token and must not
-- be interpreted as a comment. Because of this, removeing comments
-- with a preprocessor is difficult.
comment :: Parser String
comment = (do a <- (string "/*")
              b <- manyTill anyChar (try (string "*/"))
              return $ (a ++ b ++ "*/")) <?> "comment"

-- | Parse a number.
--
-- > [0-9]+|[0-9]*\.[0-9]+
--
-- Numbers may have the follwing format:
--
-- > 42, 4.2 or .42
--
num :: Parser String
num = try (do a <- many digit
              b <- string "."
              c <- many1 digit
              return $ (a ++ b ++ c)) <|> (many1 digit) <?> "number"

-- | Parse whitespace.
--
-- > {s}?
--
-- Whitespace is 0 or 1 spaces, remembering that spaces have 1 or more
-- space characters.
w :: Parser String
w = (do x <- many s
        return $ concat x) 
    <?> "whitespace"

-- | Helper function for parsing spaces. Parse a single space character.
s_char :: Parser String
s_char =   string " "
       <|> string "\t"
       <|> string "\r"
       <|> string "\n"
       <|> string "\f"
       <?> "space character"

-- | Parse spaces.
--
-- > [ \t\r\n\f]+
--
-- Spaces are 1 or more space characters.
s :: Parser String
s = (do x <- many1 s_char
        return $ concat x) 
    <?> "spaces"

-- | Parse a single new line.
--
-- > \n|\r\n|\r|\f
--
nl :: Parser String
nl =   try (string "\r\n")
   <|> string "\n"
   <|> string "\r"
   <|> string "\f"
   <?> "new line"

-- | Run a parser on a CSS formatted string.
run :: Show a => Parser a -> String -> Either ParseError a
run p input = parse p "" input
