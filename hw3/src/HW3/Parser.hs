module HW3.Parser (parse) where

import Text.Megaparsec (MonadParsec (eof, label, notFollowedBy, try), ParseErrorBundle, Parsec,
                        between, choice, empty, manyTill, satisfy, sepBy, sepBy1, (<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, hexDigitChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Applicative (Alternative (many), Applicative (liftA2))
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR, Postfix), makeExprParser)

import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..),
                 HiValueConvertable (..))

import qualified Data.Bits as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.Foldable (fold)
import Data.List (intercalate)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Void (Void)

-- | Hi language parser.
type Parser = Parsec Void String

-- | Parses Hi language expression.
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = MP.parse (between space eof pExpr) ""

--------------------------------------------------------------------------------
---- HiExpr parsers
--------------------------------------------------------------------------------

-- | HiExpr parser.
pExpr :: Parser HiExpr
pExpr = makeExprParser pApplication operatorsTable

-- | Table with parsers of Hi language operators ordered by priority (See 'makeExprParser').
operatorsTable :: [[Operator Parser HiExpr]]
operatorsTable =
        [ [mul, div] -- hs priority = 7
        , [add, sub] -- hs priority = 6
        , [equals, notEquals, less, greater, notLess, notGreater] -- hs priority = 4
        , [and] -- hs priority = 3
        , [or] -- hs priority = 2
        ]
        where
          mul        = InfixL $ binary HiFunMul            <$ symbol "*"
          div        = InfixL $ binary HiFunDiv            <$ (try . lexeme) (char '/' <* notFollowedBy (char '=')) -- to avoid ambigous with `/=`
          add        = InfixL $ binary HiFunAdd            <$ symbol "+"
          sub        = InfixL $ binary HiFunSub            <$ symbol "-"
          equals     = InfixN $ binary HiFunEquals         <$ symbol "=="
          notEquals  = InfixN $ binary HiFunNotEquals      <$ symbol "/="
          less       = InfixN $ binary HiFunLessThan       <$ (try . lexeme) (char '<' <* notFollowedBy (char '=')) -- to avoid ambigous with `<=`
          greater    = InfixN $ binary HiFunGreaterThan    <$ (try . lexeme) (char '>' <* notFollowedBy (char '=')) -- to avoid ambigous with `>=`
          notLess    = InfixN $ binary HiFunNotLessThan    <$ symbol ">="
          notGreater = InfixN $ binary HiFunNotGreaterThan <$ symbol "<="
          and        = InfixR $ binary HiFunAnd            <$ symbol "&&"
          or         = InfixR $ binary HiFunOr             <$ symbol "||"

-- | Creates binary operator for 'HiExpr' from 'HiFun'.
binary :: HiFun -> HiExpr -> HiExpr -> HiExpr
binary fun arg1 arg2 = HiExprApply (HiExprValue $ HiValueFunction fun) [arg1, arg2]

--------------------------------------------------------------------------------
---- Application parser
--------------------------------------------------------------------------------

{-
Simplified grammar for parsing application:

Application     ::= ApplicationToken ApplicationRest
ApplicationRest ::= '(' Expr ',' ... ')' ApplicationRest
ApplicationRest ::= '!' ApplicationRest
ApplicationRest ::= '.' DotAccessKey ApplicationRest
ApplicationRest ::= eps
-}

-- | Parses Application which is any ApplicationToken with several number of @()@, @!@ or @.@ applied to it.
pApplication :: Parser HiExpr
pApplication = do
  value <- pApplicationToken
  rest  <- pApplicationRest
  return $ rest value

-- | Parses rest of Application which is several number of @()@, @!@ or @.@ applied to some ApplicationToken.
pApplicationRest :: Parser (HiExpr -> HiExpr)
pApplicationRest = notEmptyRest <|> emptyRest
  where
    funCall = do
      args <-  parens $ pExpr `sepBy` symbol ","
      return $ \fun -> HiExprApply fun args
    runAction = HiExprRun <$ symbol "!"
    dotAccess = do
      char '.'
      key <- lexeme $ intercalate "-" <$> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
      return $ \app -> HiExprApply app [HiExprValue $ toHiValue (T.pack key)]

    notEmptyRest = do
      app  <- funCall <|> runAction <|> dotAccess
      post <- pApplicationRest
      return $ \pre -> post (app pre)
    emptyRest = pure id

-- | Parses ApplicationToken which is one of literals (number, string, list, ...) or Expr in parentheses.
-- @()@, @!@ or @.@ can be applied to it.
pApplicationToken :: Parser HiExpr
pApplicationToken = (HiExprValue <$> pHiValue <|> pList <|> pDict) <|> parens pExpr

-- | Parses a list.
pList :: Parser HiExpr
pList = label "list" $ HiExprApply (HiExprValue (HiValueFunction HiFunList))
  <$> between (char '[' *> notFollowedBy (char '#') *> space) (symbol "]") (pExpr `sepBy` symbol ",")

-- | Parses a dictionary.
pDict :: Parser HiExpr
pDict  = label "dict" $ HiExprDict <$> between (symbol "{") (symbol "}") (dictEntry `sepBy` symbol ",")
  where
    dictEntry = do
      fstElem <- pExpr
      symbol ":"
      sndElem <- pExpr
      return (fstElem, sndElem)

--------------------------------------------------------------------------------
---- HiValue parsers
--------------------------------------------------------------------------------

-- Parses a HiValue.
pHiValue :: Parser HiValue
pHiValue = number <|> bool <|> fun <|> nullLiteral <|> stringLiteral <|> bytesLiteral <|> cwd <|> now

-- | Parses a number.
number :: Parser HiValue
number = label "number" $ HiValueNumber . toRational <$> L.signed space (lexeme L.scientific)

-- | Parses a boolean.
bool :: Parser HiValue
bool = label "boolean" $ HiValueBool <$> (False <$ symbol "false" <|> True <$ symbol "true")

-- | Parses a function identifier.
fun :: Parser HiValue
fun = label "function" $ HiValueFunction <$> choice
  [ HiFunAdd            <$ symbol "add"
  , HiFunSub            <$ symbol "sub"
  , HiFunMul            <$ symbol "mul"
  , HiFunDiv            <$ symbol "div"
  , HiFunNot            <$ (try . lexeme) (string "not" <* notFollowedBy (char '-')) -- to avoid ambigous with @not-less-than@, @not-greater-than@, @not-equals@
  , HiFunAnd            <$ symbol "and"
  , HiFunOr             <$ symbol "or"
  , HiFunLessThan       <$ symbol "less-than"
  , HiFunGreaterThan    <$ symbol "greater-than"
  , HiFunEquals         <$ symbol "equals"
  , HiFunNotLessThan    <$ try (symbol "not-less-than")
  , HiFunNotGreaterThan <$ try (symbol "not-greater-than")
  , HiFunNotEquals      <$ try (symbol "not-equals")
  , HiFunIf             <$ symbol "if"
  , HiFunLength         <$ symbol "length"
  , HiFunToUpper        <$ symbol "to-upper"
  , HiFunToLower        <$ symbol "to-lower"
  , HiFunReverse        <$ symbol "reverse"
  , HiFunTrim           <$ symbol "trim"
  , HiFunList           <$ symbol "list"
  , HiFunRange          <$ symbol "range"
  , HiFunFold           <$ symbol "fold"
  , HiFunPackBytes      <$ symbol "pack-bytes"
  , HiFunUnpackBytes    <$ symbol "unpack-bytes"
  , HiFunEncodeUtf8     <$ symbol "encode-utf8"
  , HiFunDecodeUtf8     <$ symbol "decode-utf8"
  , HiFunZip            <$ symbol "zip"
  , HiFunUnzip          <$ symbol "unzip"
  , HiFunSerialise      <$ symbol "serialise"
  , HiFunDeserialise    <$ symbol "deserialise"
  , HiFunRead           <$ symbol "read"
  , HiFunWrite          <$ symbol "write"
  , HiFunMkDir          <$ symbol "mkdir"
  , HiFunChDir          <$ symbol "cd"
  , HiFunParseTime      <$ symbol "parse-time"
  , HiFunRand           <$ symbol "rand"
  , HiFunEcho           <$ symbol "echo"
  , HiFunCount          <$ symbol "count"
  , HiFunKeys           <$ symbol "keys"
  , HiFunValues         <$ symbol "values"
  , HiFunInvert         <$ symbol "invert"
  ]

-- | Parses a null.
nullLiteral :: Parser HiValue
nullLiteral = label "null" $ HiValueNull <$ symbol "null"

-- | Parses a string literal.
stringLiteral :: Parser HiValue
stringLiteral = label "string literal" $ HiValueString . T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

-- | Parses a bytes literal.
bytesLiteral :: Parser HiValue
bytesLiteral = label "bytes literal" $ lexeme $ HiValueBytes . BS.pack
  <$> between (symbol "[#") (symbol "#]") (lexeme (word8 `sepBy` try (space1 *> notFollowedBy (char '#'))))
  where
    word8 = do
      l <- hexDigitChar
      r <- hexDigitChar
      let lw = digitToInt l
      let rw = digitToInt r
      return $ fromIntegral (16 * lw + rw)

-- | Parses cwd keyword.
cwd :: Parser HiValue
cwd = HiValueAction HiActionCwd <$ symbol "cwd"

-- | Parses now keyword.
now :: Parser HiValue
now = HiValueAction HiActionNow <$ symbol "now"

--------------------------------------------------------------------------------
---- Utilities
--------------------------------------------------------------------------------

-- | Parses zero or more spaces.
space :: Parser ()
space = L.space space1 empty empty

-- | Wrapper for parsers, handling spaces after parsed text.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

-- | Parser of special string, handling spaces after parsed text.
symbol :: String -> Parser String
symbol = L.symbol space

-- | Parses same text as inner parser but wrapped in parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
