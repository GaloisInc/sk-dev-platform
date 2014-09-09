{-# LANGUAGE ScopedTypeVariables #-}
--module SCD.Analysis.XML where

import Text.XML.Light(parseXML, showElement, strContent, elChildren,
  unqual, Content(..), Element(..), Attr(..), cdData, Line)

import Text.Regex.Posix((=~))
import Data.Char(isSpace)

import Control.Monad(when)
import System.Environment(getArgs)

import Data.List.GroupSort(groupSort)

import Text.ParserCombinators.PolyReadP(ReadP, parse, option,
  optional, many, get, munch)

import Text.PrettyPrint.HughesPJ(render, nest, ($+$), text, empty, Doc, (<+>),(<>),parens,colon, sep)

data InterfaceModule = InterfaceModule{ file :: String
                                      , moduleSummary :: String
                                      , moduleDescription :: [Content]
                                      , required :: Bool
                                      , requiredDescription :: String
                                      , interfaces :: [Interface]
                                      }
  deriving Show

data Interface = Interface{ interfaceSummary :: String
                          , interfaceDescription :: [Content]
                          , parameters :: [Parameter]
                          , infoFlow :: Maybe InfoFlow
                          }
  deriving Show

data InfoFlow = InfoFlow{ flow :: Flow
                        , weight :: Int
                        } 
  deriving (Eq, Read, Show)

data Flow = Read | Write | Both | None
  deriving (Eq, Read, Show)


data Parameter = Parameter{ line :: Maybe Line
                          , name :: String
                          , parameterSummary :: String
                          }
  deriving (Eq, Read, Show)

type P a = ReadP [Content] Content a

withContent :: Show a => [Content] -> P a -> P a
withContent s p = either (\e -> fail e) return $ parse p s

parseFile :: String -> IO (Either String InterfaceModule)
parseFile f = do
  s <- readFile f
  return (parseModule f s)

main :: IO ()
main = do
   ca:fs <- getArgs
   c <- case ca of "--per-parameter" -> return collect
                   "--all-parameters" -> return collectAllParams
                   _                  -> fail $ "Unknown flag: " ++ show ca
   mapM (\f -> parseFile f >>= either (fail.show.show) return) fs >>= (putStrLn . c)

above :: [Doc] -> Doc
above = foldr ($+$) empty

collect :: [InterfaceModule] -> String
collect ms = render $ above
           $ map (\(n,l) -> text "******" <+> text n $+$ 
                             (nest 4 $ above $ 
                                map (\(s,l') -> 
                                     text (unwords (lines s)) $+$ nest (-4) (
                                     above ([text (file m) <> colon <> text (show (line p)) <> colon <+> text n <+> parens (text (unwords (lines s)))
                                                           | (m,_i,p) <- l'])))
                                (groupSort (\(_m,_i,p)->
                                              dropWhile isSpace $
                                              parameterSummary p) l)))
           $ groupSort (\(_m,_i,p) -> name p)
           $ concat (concat [[[(m,i,p) | p <- parameters i] 
                                  | i <- interfaces m] 
                             | m <- ms])

collectAllParams :: [InterfaceModule] -> String
collectAllParams ms = render $ above
           $ map (\(ns,l) -> let dns = sep (map text ns) in 
                             text "******" <+> dns $+$ 
                             (above $ 
                                map (\(m,i) -> text (file m) <> colon <> text (show (line( head (parameters i)))) <> colon <+> text (unwords (lines (interfaceSummary i)))) l))
           $ groupSort (\(_m,i) -> map name (parameters i))
           $ concat ([[(m,i) | i <- interfaces m] 
                             | m <- ms])

parseModule :: String -> String -> Either String InterfaceModule
parseModule filename = parse (parseModule' filename) . parseXML 
                     . extractXML

-- from refpolicy/support/segenxml.py: XML_COMMENT = re.compile("^##\s+(.*?)\s*$")

extractXML :: String -> String
extractXML l = unlines [after | (_before::String,_pattern::String,after) <- map (=~ "^##\\s") (lines l)]

parseModule' :: String -> P InterfaceModule
parseModule' filename = do
  s <- parseSummary
  d <- parseDescription
  (r,rd) <- option (False,"") parseRequired
  is <- many parseInterface
  return InterfaceModule{ file = filename
                        , moduleSummary = s
                        , moduleDescription = d
                        , required = r
                        , requiredDescription = rd
                        , interfaces = is
                        } 

parseSummary :: P String
parseSummary = do
  s <- parseElement "summary"
  when (not (null (elChildren s))) $ 
       fail $ "Illegal content in summary: " ++ showElement s
  return (strContent s)

parseDescription :: P [Content]
parseDescription = option [] (elContent `fmap` parseElement "desc")
      
parseInterface :: P Interface
parseInterface = do
  s <- parseSummary
  d <- parseDescription
  ps <- many parseParameter
  i <- option Nothing (Just `fmap` parseInfoFlow)
  optional (parseElement "rolecap")
  optional (parseElement "rolebase")
  return Interface{ interfaceSummary = s
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
  ws <- parseAttr "weight" p
  w <- case reads ws of 
         [(a,"")] -> return a
         _        -> fail $ "Illegal infoflow weight: " ++ show ws
  return InfoFlow{ flow = f, weight = w }

parseParameter :: P Parameter
parseParameter = do
  p <- parseElement "param"
  n <- parseAttr "name" p
  s <- withContent (elContent p) parseSummary 
  return Parameter{ line = elLine p
                  , name = n
                  , parameterSummary = s
                  }

parseRequired :: P (Bool,String)
parseRequired = do
  p <- parseElement "required"
  n <- parseAttr "val" p
  r <- case n of
         "true"  -> return True
         "false" -> return False
         _       -> fail $ "Illegal boolean: " ++ show n
  return (r, strContent p)
  
parseElement :: String -> P Element
parseElement t = do
  eatWhiteSpace
  e <- tok (\c -> 
             case c of Elem e | elName e == unqual t -> Just e
                       _ -> Nothing)
  eatWhiteSpace
  return e

eatWhiteSpace :: P [Content]
eatWhiteSpace = munch $ \c ->
                case c of 
                  Text cd | all isSpace (cdData cd) -> True
                  _ -> False

parseAttr :: String -> Element -> P String
parseAttr t e = case [attrVal a | a <- elAttribs e, attrKey a == unqual t] of
                  v:_ -> return v
                  []  -> fail $ "Expected attribute "++show t++" at "++
                                showElement e

tok :: (Content -> Maybe a) -> P a
tok p = get >>= (maybe (fail "token") return . p)
