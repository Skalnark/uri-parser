{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Parse(test)
  where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

test :: Text -> IO ()
test inputStr = parseTest (pUri <* eof) inputStr

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

data Uri = Uri 
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  , path :: Maybe Path
  } deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text) -- user, password
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)

data Path = Path
  { pathSegments :: Text
  , pathExtension :: Maybe Text
  , pathQuery :: Maybe [PathVar]
  , pathFragment :: Maybe Text
  } deriving (Eq, Show)

data PathVar = PathVar
  { pathVarName :: Text
  , pathVarValue :: Text
  } deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme <?> "valid scheme"
  void (char ':')
  uriAuthority <- optional $ do
    void (string "//")
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar <?> "username"
      void (char ':')
      password <- T.pack <$> some alphaNumChar <?> "password"
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
    authPort <- optional (char ':' *> label "port number" L.decimal)
    return Authority {..}
  void (char '/')
  path <- optional $ do
    pathSegments <- T.pack <$> some (alphaNumChar <|> char '/') <?> "path"
    pathExtension <- optional $ do
      void (char '.')
      T.pack <$> some alphaNumChar <?> "extension"
    pathQuery <- optional $ do
      void (char '?')
      sepBy pathVar (char '&')
    pathFragment <- optional $ do
      void (char '#')
      T.pack <$> some alphaNumChar <?> "fragment"
    return Path {..}
  return Uri {..}

pathVar :: Parser PathVar
pathVar = do
  pathVarName <- T.pack <$> some alphaNumChar <?> "query variable name"
  void (char '=')
  pathVarValue <- T.pack <$> some (anySingleBut '&') <?> "query variable value"
  return PathVar {..}