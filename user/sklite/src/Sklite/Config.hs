{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Sklite.Config
    ( parseLayoutConfig
    , parseLayoutFile
    )
where

import Control.Applicative
import Data.Char (toLower)
import Text.XML.Light

import Sklite.Types

name :: String -> QName
name n = QName { qName = n
               , qURI = Nothing
               , qPrefix = Nothing
               }

-- Element names
eLayout = name "layout"
eDefineSegment = name "segment"
eCell = name "cell"
eUseSegment = name "use-segment"
eArgs = name "args"
eArg = name "arg"
eChannel = name "channel"
eExternalInterface = name "external_interface"

-- Attribute names
-- layout:
aBandwidth = name "bandwidth"

-- segment:
aName = name "name"
aSize = name "size"

-- cell:
aProgram = name "program"
aRuntime = name "runtime"
aPeriod = name "period"
aUser = name "user"
aEntryAddr = name "entryAddr"
aBinary = name "binary"

-- channels:
aMsgsize = name "msgsize"
aMsgslots = name "slots"
aFrom = name "from"
aTo = name "to"
aOverwrite = name "overwrite"

-- arg:
aValue = name "value"

-- use-segment:
aAlias = name "alias"
aPrivileges = name "privileges"
aMapTo = name "mapto"

-- external_interface:
aInterface = name "interface"

parseLayoutFile :: FilePath -> IO (Either String Layout)
parseLayoutFile path = parseLayoutConfig <$> readFile path

parseLayoutConfig :: String -> Either String Layout
parseLayoutConfig s = parseRoot =<< (findRoot $ parseXML s)

findRoot :: [Content] -> Either String Element
findRoot [] = Left "No root element found"
findRoot ((Elem e):es)
    | elName e == eLayout = Right e
    | otherwise = findRoot es
findRoot (_:es) = findRoot es

parseRoot :: Element -> Either String Layout
parseRoot e
    | elName e == eLayout =
        Layout <$> parseCells e <*> parseDefinedSegments e <*>
               parseBandwidth e <*> parseChannels e
    | otherwise = Left $ "Root element is not " ++ show eLayout

parseChannels :: Element -> Either String [Channel]
parseChannels e = do
  let isChan = (== eChannel) . elName
      findChanElements c = filter isChan (contentElements c)
      cs = findChanElements e
      parseChan c = do
                    c1 <- Channel <$> requiredAttr aFrom c <*> requiredAttr aTo c <*>
                          parseMsgsize c <*> parseMsgslots c <*> parseName c <*> parseOverwrite c
                    return [c1]

  concat <$> mapM parseChan cs

parseCells :: Element -> Either String [Cell]
parseCells e = do
  let findCellElements c = filter ((== eCell) . elName) (contentElements c)
      cs = findCellElements e
      parseCell c = Cell <$> parseName c <*> parseCellUser c <*>
                    parseCellProgram c <*> parseCellRunMethod c <*> parseRuntime c <*> parsePeriod c <*>
                    parseUsedSegments c <*> parseCellArgs c <*> parseExternalInterfaces c

  mapM parseCell cs

parseCellRunMethod :: Element -> Either String CellRunMethod
parseCellRunMethod e =
    let v = optionalAttr aEntryAddr e Nothing Just
    in case v of
         Nothing -> Right CellMain
         Just s ->
             case parseNum aEntryAddr s of
                 Left e' -> Left e'
                 Right i -> do
                     v' <- requiredAttr aBinary e
                     s' <- requiredAttr aSize e
                     case parseNum aSize s' of
                         Left e'' -> Left e''
                         Right i' -> return $ RawBinary i v' i'

parseCellArgs :: Element -> Either String [String]
parseCellArgs e = do
  let findArgsElements c = filter ((== eArgs) . elName) (contentElements c)
      findArgElements c = filter ((== eArg) . elName) (contentElements c)
      as = findArgsElements e

  case as of
    [] -> Right []
    [elm] -> mapM (requiredAttr aValue) (findArgElements elm)
    _ -> Left "At most one 'args' element permitted in each cell"

parseExternalInterfaces :: Element -> Either String [String]
parseExternalInterfaces e =
  let findExternalInterfaceElements c = filter ((== eExternalInterface) . elName) (contentElements c)
  in mapM parseInterface (findExternalInterfaceElements e)

parseInterface :: Element -> Either String String
parseInterface = requiredAttr aInterface

parseName :: Element -> Either String String
parseName = requiredAttr aName

parseCellUser :: Element -> Either String String
parseCellUser = requiredAttr aUser

parseCellProgram :: Element -> Either String FilePath
parseCellProgram = requiredAttr aProgram

parseUsedSegments :: Element -> Either String [SharedMemoryAccess]
parseUsedSegments e = do
  let findAcessElements c = filter ((== eUseSegment) . elName) (contentElements c)
      cs = findAcessElements e
      parseAccess c = SharedMemoryAccess <$> parsePrivs c <*> parseName c <*>
                      parseAlias c <*> parseMapTo c

  mapM parseAccess cs

privOptions :: [(String, AccessType)]
privOptions =
    [ ("ro", MemReadOnly)
    , ("rw", MemReadWrite)
    , ("wo", MemWriteOnly)
    ]

parsePrivs :: Element -> Either String AccessType
parsePrivs e = do
  v <- requiredAttr aPrivileges e
  case lookup v privOptions of
    Nothing -> Left $ concat[ "Invalid privilege string: "
                            , show v
                            , ", must be one of "
                            , show $ fst <$> privOptions
                            ]
    Just t -> return t

parseAlias :: Element -> Either String String
parseAlias = requiredAttr aAlias

parseMapTo :: Element -> Either String (Maybe Integer)
parseMapTo e = do
    case optionalAttr aMapTo e Nothing Just of
        Nothing -> Right Nothing
        Just s -> do
            n <- parseNum aMapTo s
            return $ Just n

affirmative :: String -> Bool
affirmative s =
    case (toLower <$> s) of
      "yes" -> True
      "1" -> True
      "true" -> True

      "no" -> False
      "0" -> False
      "false" -> False

      _ -> False

parseOverwrite :: Element -> Either String Bool
parseOverwrite e =
    Right $ affirmative $ optionalAttr aOverwrite e "no" id

contentElements :: Element -> [Element]
contentElements e = contentEs $ elContent e
    where
      contentEs [] = []
      contentEs (Elem c:cs) = c : contentEs cs
      contentEs (_:cs) = contentEs cs

parseDefinedSegments :: Element -> Either String [SharedMemoryRegion]
parseDefinedSegments e = do
  let findRegions c = filter ((== eDefineSegment) . elName) (contentElements c)
      cs = findRegions e
      parseRegion c = SharedMemoryRegion <$> parseSize c <*> parseName c
      parseSize c = parseNum aSize =<< requiredAttr aSize c

  mapM parseRegion cs

parseBandwidth :: Element -> Either String Double
parseBandwidth e = parseNum aBandwidth =<< requiredAttr aBandwidth e

parseRuntime :: Element -> Either String Double
parseRuntime e = parseNum aRuntime =<< requiredAttr aRuntime e

parsePeriod :: Element -> Either String Double
parsePeriod e = parseNum aPeriod =<< requiredAttr aPeriod e

parseMsgsize :: Element -> Either String Integer
parseMsgsize e = parseNum aMsgsize =<< requiredAttr aMsgsize e

parseMsgslots :: Element -> Either String Integer
parseMsgslots e = parseNum aMsgslots =<< requiredAttr aMsgslots e

requiredAttr :: QName -> Element -> Either String String
requiredAttr n e =
    case filter ((== n) . attrKey) (elAttribs e) of
      [] -> Left $ concat [ "Required attribute "
                          , show $ qName n
                          , " missing from element "
                          , show $ qName $ elName e
                          ]
      (val:_) -> Right $ attrVal val

optionalAttr :: QName -> Element -> a -> (String -> a) -> a
optionalAttr n e def f =
    case filter ((== n) . attrKey) (elAttribs e) of
      [] -> def
      (val:_) -> f $ attrVal val

parseNum :: (Read a, Num a) => QName -> String -> Either String a
parseNum nam val =
    case reads val of
      [] -> Left $ "Value of " ++ (show $ qName nam) ++ " attribute must be an integer"
      ((i,_):_) -> Right i
