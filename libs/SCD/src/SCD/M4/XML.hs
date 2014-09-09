{-# LANGUAGE ScopedTypeVariables #-}
{- |
Module      : $Header$
Description : Utility functions for XML extraction from policies
Copyright   : (c) Galois, Inc.

Utility functions for XML extraction from policies
-}

module SCD.M4.XML(P, parseModule, parseInterface, extract) where

import Text.XML.Light(showElement, unqual, Content(..), Element(..),
  Attr(..), cdData, strContent, elChildren)

import Text.Regex.Posix((=~))
import Data.Char(isSpace)

import Control.Monad(when)

import Text.ParserCombinators.PolyReadP(ReadP, parse, option,
  many, get, munch)

import qualified Text.ParserCombinators.PolyReadP as PR

import SCD.M4.Syntax(ModuleDoc(..), InterfaceDoc(..), InfoFlow(..),
  Flow(..), Parameter(..))

type P a = ReadP [Content] Content a

withContent :: Show a => [Content] -> P a -> P a
withContent s p = either (\e -> fail e) return $ parse p s

-- from refpolicy/support/segenxml.py: XML_COMMENT = re.compile("^##\s+(.*?)\s*$")

extract :: String -> String
extract l = unlines [after | (_before::String,_pattern::String,after) <- map (=~ "^##[[:space:]]") (lines l)]

parseModule :: P ModuleDoc
parseModule = do
  s <- parseSummary
  d <- parseDescription
  (r,rd) <- option (False,"") parseRequired
  return ModuleDoc{ moduleSummary = s
                  , moduleDescription = d
                  , required = r
                  , requiredDescription = rd
                  }

parseSummary :: P String
parseSummary = do
  s <- parseElement "summary"
  when (not (null (elChildren s))) $
       fail $ "Illegal content in summary: " ++ showElement s
  return (strContent s)

parseDescription :: P [Content]
parseDescription = option [] (elContent `fmap` parseElement "desc")

parseInterface :: P InterfaceDoc
parseInterface = do
  s <- parseSummary
  d <- parseDescription
  ps <- many parseParameter
  i <- option Nothing (Just `fmap` parseInfoFlow)
  PR.optional (parseElement "rolecap")
  PR.optional (parseElement "rolebase")
  return InterfaceDoc{ interfaceSummary = s
                     , interfaceDescription = d
                     , parameters = ps
                     , infoFlow = i
                     }

parseInfoFlow :: P InfoFlow
parseInfoFlow = do
  p <- parseElement "infoflow"
  fs <- parseAttr "type" p
  f <- case fs of
         "read"  -> return Read
         "write" -> return Write
         "none"  -> return None
         "both"  -> return Both
         _       -> fail $ "Illegal infoflow type: " ++ show fs
  w <- if f == None then return 0 else do
       ws <- parseAttr "weight" p
       case reads ws of
         [(a,"")] -> return a
         _        -> fail $ "Illegal infoflow weight: " ++ show ws
  return InfoFlow{ flow = f, weight = w }

parseParameter :: P Parameter
parseParameter = do
  p <- parseElement "param"
  n <- parseAttr "name" p
  o <- parseAttr' parseBool (Just False) "optional" p
  s <- withContent (elContent p) parseSummary
  return Parameter{ line = elLine p
                  , name = n
                  , parameterSummary = s
                  , optional = o
                  }

parseRequired :: P (Bool,String)
parseRequired = do
  p <- parseElement "required"
  r <- parseAttr' parseBool Nothing "val" p
  return (r, strContent p)

parseBool :: String -> P Bool
parseBool n = case n of
                "true"  -> return True
                "false" -> return False
                _       -> fail $ "Illegal boolean: " ++ show n

parseElement :: String -> P Element
parseElement t = do
  _ <- eatWhiteSpace
  e <- tok (\c ->
             case c of Elem e | elName e == unqual t -> Just e
                       _ -> Nothing)
  _ <- eatWhiteSpace
  return e

eatWhiteSpace :: P [Content]
eatWhiteSpace = munch $ \c ->
                case c of
                  Text cd | all isSpace (cdData cd) -> True
                  _ -> False

parseAttr :: String -> Element -> P String
parseAttr = parseAttr' return Nothing

parseAttr' :: (String -> P a) -> Maybe a -> String -> Element -> P a
parseAttr' p md t e =
  case [attrVal a | a <- elAttribs e, attrKey a == unqual t] of
    v:_ -> p v
    []  -> maybe (fail $ "Expected attribute "++show t++" at "++
                  showElement e) return md

tok :: (Content -> Maybe a) -> P a
tok p = get >>= (maybe (fail "token") return . p)
