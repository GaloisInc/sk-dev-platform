{- |
Module      :  $Header$
Description :  Tests the SELinux authorization relation
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Tests the SELinux authorization relation
-}
module SCD.SELinux.Test.Authorize
    ( testCases
    , testDirectory
    )
where

{-import Debug.Trace(trace)-}

import qualified System.Directory
import qualified System.IO
import qualified System.IO.Error
import qualified System.Process
import qualified Text.ParserCombinators.ReadP as ReadP
import Control.Monad ({-ap,foldM,-}liftM)
import Control.Arrow ( left, right )
import Control.Monad.Error ()
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
--import System.Posix.POpen(popen)
import SCD.SELinux.Syntax
import SCD.SELinux.Monad


import qualified SCD.SELinux.Symbol as Symbol
import qualified SCD.SELinux.Test.Symbol as Symbol        ( tryBuildSymbolTable )
import qualified SCD.SELinux.Authorize as Authorize
import qualified SCD.SELinux.Test.Parser as SELinuxParser ( tryParsePolicyFile, parsePolicyFile )


import Prelude hiding (FilePath)
import System.Environment(getEnv)

import Test.HUnit                     ( Assertion, assertFailure )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework                 ( Test, testGroup )

quotify :: String -> String
quotify x = if elem ' ' x then "'" ++ x ++ "'" else x

platform :: IO String
platform =
    do (_,out,_,_) <-
           System.Process.runInteractiveProcess "uname" [] Nothing Nothing
       s <- System.IO.hGetContents out
       return (filter (not . Char.isSpace) s)

tryBuildAuthorize :: Policy -> Symbol.SymbolTable ->
                     Either String Authorize.Authorize
tryBuildAuthorize = Authorize.build

buildAuthorize :: String -> Policy -> Symbol.SymbolTable -> Authorize.Authorize
buildAuthorize filename policy symbol =
    case tryBuildAuthorize policy symbol of
      Left err ->
          error ("ERROR: couldn't build the authorization relation for " ++
                 filename ++ ":\n" ++ err)
      Right auth ->
          auth

tryBuildAuthorizeFile ::
    String -> IO (Either String (Policy, Symbol.SymbolTable, Authorize.Authorize))
tryBuildAuthorizeFile filename =
    do parseResult <- SELinuxParser.tryParsePolicyFile filename
       case parseResult of
         Left err -> return (Left (err ++ " (parse error)"))
         Right policy ->
             case Symbol.tryBuildSymbolTable policy of
               Left err -> return (Left (err ++ " (scope error)"))
               Right (policy',symbol) ->
                   case tryBuildAuthorize policy' symbol of
                     Left err -> return (Left (err ++ " (authorization error)"))
                     Right auth -> return (Right (policy',symbol,auth))

-- buildAuthorizeFile ::
--     String -> IO (Policy, Symbol.SymbolTable, Authorize.Authorize)
-- buildAuthorizeFile filename =
--     do buildResult <- tryBuildAuthorizeFile filename
--        case buildResult of
--          Left err ->
--              error ("ERROR: couldn't process policy file " ++
--                     filename ++ ":\n" ++ err)
--          Right p_s_a -> return p_s_a

data Command =
    BoolCommand BoolId Bool
  | AccessCommand SecurityContext SecurityContext ClassId
  | TransitionCommand SecurityContext SecurityContext SecurityContext ClassId
  | DefaultTransitionCommand
      SecurityContext SecurityContext SecurityContext ClassId
    deriving (Eq, Read, Show)

prettyPrintSecurityContext :: SecurityContext -> String
prettyPrintSecurityContext (SecurityContext u r t) =
    idString u ++ ":" ++ idString r ++ ":" ++ idString t

prettyPrintCommand :: Command -> String
prettyPrintCommand cmd =
    case cmd of
      BoolCommand bi bv -> "BOOL ( " ++ idString bi ++ " := " ++ show bv ++ " )"
      AccessCommand subj obj cl ->
          "ACCESS ( " ++ prettyPrintSecurityContext subj ++ " " ++
          prettyPrintSecurityContext obj ++ " " ++ idString cl ++ " )"
      TransitionCommand subj obj new cl ->
          "TRANSITION ( " ++ prettyPrintSecurityContext subj ++ " " ++
          prettyPrintSecurityContext obj ++ " " ++
          prettyPrintSecurityContext new ++ " " ++ idString cl ++ " )"
      DefaultTransitionCommand subj obj new cl ->
          "DEFAULT_TRANSITION ( " ++ prettyPrintSecurityContext subj ++ " " ++
          prettyPrintSecurityContext obj ++ " " ++
          prettyPrintSecurityContext new ++ " " ++ idString cl ++ " )"

parseChar :: Char -> ReadP.ReadP ()
parseChar c =
    do _ <- ReadP.char c
       return ()

parseNewline :: ReadP.ReadP ()
parseNewline = parseChar '\n'

parseSpace :: ReadP.ReadP ()
parseSpace = parseChar ' '

parseEof :: ReadP.ReadP ()
parseEof =
    do s <- ReadP.look
       if s == "" then return () else ReadP.pfail

parseIdentifier :: IsIdentifier i => ReadP.ReadP i
parseIdentifier =
    do h <- ReadP.satisfy Char.isAlpha
       t <- ReadP.munch (\c -> Char.isAlphaNum c || c == '_')
       return (mkId (h:t))

parseSecurityContext :: ReadP.ReadP SecurityContext
parseSecurityContext =
    do u <- parseIdentifier
       parseChar ':'
       r <- parseIdentifier
       parseChar ':'
       t <- parseIdentifier
       return (SecurityContext u r t)

parseSet :: ReadP.ReadP a -> ReadP.ReadP [a]
parseSet parser =
    do parseChar '{'
       ReadP.skipSpaces
       ps <- ReadP.sepBy parser ReadP.skipSpaces
       ReadP.skipSpaces
       parseChar '}'
       return ps

parseFirstParsableLine :: ReadP.ReadP a -> ReadP.ReadP a
parseFirstParsableLine parser =
    parser ReadP.<++
    do ReadP.skipMany (ReadP.satisfy (\c -> c /= '\n'))
       parseNewline
       parseFirstParsableLine parser

parseCommands :: ReadP.ReadP [Command]
parseCommands =
    do cmdos <- ReadP.manyTill commando parseEof
       return (Maybe.catMaybes cmdos)
    where
      commando =
          (command >>= return . Just) ReadP.<++
          (noncommand >> return Nothing)

      command =
          do _ <- ReadP.char '#'
             ReadP.choice [boolCommand,accessCommand,
                           transitionCommand,defaultTransitionCommand]

      noncommand =
          do ReadP.skipMany (ReadP.satisfy (\c -> c /= '\n'))
             parseNewline

      boolCommand =
          do _ <- ReadP.string "BOOL"
             separator
             boolCommandArgs ReadP.<++ error "bad arguments to BOOL command"

      boolCommandArgs =
          do b <- parseIdentifier
             separator
             v <- boolValue
             endOfArgs
             return (BoolCommand b v)

      boolValue =
          (ReadP.string "true" >> return True) ReadP.+++
          (ReadP.string "false" >> return False)

      accessCommand =
          do _ <- ReadP.string "ACCESS"
             separator
             accessCommandArgs ReadP.<++ error "bad arguments to ACCESS command"

      accessCommandArgs =
          do subj <- parseSecurityContext
             separator
             obj <- parseSecurityContext
             separator
             cl <- parseIdentifier
             endOfArgs
             return (AccessCommand subj obj cl)

      transitionCommand =
          do _ <- ReadP.string "TRANSITION"
             separator
             transitionCommandArgs TransitionCommand ReadP.<++
               error "bad arguments to TRANSITION command"

      defaultTransitionCommand =
          do _ <- ReadP.string "DEFAULT_TRANSITION"
             separator
             transitionCommandArgs DefaultTransitionCommand ReadP.<++
               error "bad arguments to DEFAULT_TRANSITION command"

      transitionCommandArgs constructor =
          do subj <- parseSecurityContext
             separator
             obj <- parseSecurityContext
             separator
             new <- parseSecurityContext
             separator
             cl <- parseIdentifier
             endOfArgs
             return (constructor subj obj new cl)

      separator = ReadP.skipMany1 parseSpace

      endOfArgs =
          do ReadP.skipMany parseSpace
             parseNewline

readCommands :: String -> IO [Command]
readCommands filename =
    do policyFile <- readFile filename
       case ReadP.readP_to_S parseCommands policyFile of
         [(commands,"")] -> return commands
         x -> error ("Test.Authorize: couldn't parse test in " ++ filename ++
                     ":\n" ++ show x)

data Dfa a = Dfa {initialState :: a,
                  stateTransition :: a -> Char -> a,
                  acceptingState :: a -> Bool}

stringToDfa :: String -> Dfa [String]
stringToDfa s =
    Dfa {initialState = [s],
         stateTransition = transition,
         acceptingState = elem []}
    where
      transition l c = s : Maybe.mapMaybe (advance c) l

      advance _ [] = Nothing
      advance c (x : xs) = if c == x then Just xs else Nothing

orDfa :: Dfa a -> Dfa b -> Dfa (a,b)
orDfa dfa1 dfa2 =
    let Dfa {initialState = i1,
             stateTransition = t1,
             acceptingState = a1} = dfa1 in
    let Dfa {initialState = i2,
             stateTransition = t2,
             acceptingState = a2} = dfa2 in
    Dfa {initialState = (i1,i2),
         stateTransition = \(s1,s2) c -> (t1 s1 c, t2 s2 c),
         acceptingState = \(s1,s2) -> a1 s1 || a2 s2}

interactTimeoutSeconds :: Int
interactTimeoutSeconds = 1

waitForDfa :: System.IO.Handle -> String -> Dfa a -> IO String
waitForDfa response name dfa =
    waitFor (initialState dfa) []
    where
      waitFor state cs =
          if acceptingState dfa state then return (reverse cs) else
              do ready <- catchEof cs (System.IO.hWaitForInput
                                       response (1000 * interactTimeoutSeconds))
                 if ready then return () else
                     error (reverse cs ++ "\nblocked while waiting for " ++ name)
                 c <- catchEof cs (System.IO.hGetChar response)
{-
                 System.IO.putChar c
                 System.IO.hFlush System.IO.stdout
-}
                 waitFor (stateTransition dfa state c) (c:cs)

      catchEof cs x =
          x `System.IO.Error.catch`
          (\e -> if System.IO.Error.isEOFError e then
                     error (reverse cs ++ "\nEOF while waiting for " ++ name)
                 else System.IO.Error.ioError e)

waitForPrompt :: System.IO.Handle -> String -> IO String
waitForPrompt response prompt =
    waitForDfa response ("prompt " ++ show prompt) (stringToDfa prompt)

relatedObjectClass :: Symbol.SymbolTable -> ClassId -> ClassId
relatedObjectClass symbol cl =
    let relName =
            case idString cl of
              "process" -> "file"
              "file" -> "dir"
              n -> error ("no related object class for class \"" ++ n ++ "\"") in
    let relCl = mkId relName in
    if canRunP (Symbol.allClassPermissions symbol relCl) then relCl else
        error ("related object class \"" ++ relName ++ "\" does not exist")

executeCommands :: (String -> IO ()) -> (String -> IO String) ->
                   Symbol.SymbolTable -> Maybe Authorize.Authorize ->
                   [Command] -> IO ()
executeCommands issue responses symbol =
    \autho cmds ->
        do _ <- mainPrompt
           execute autho cmds
    where
      mainPrompt = responses "Choose:  "

      execute _ [] = issue "q"
      execute autho (cmd:cmds) =
          case cmd of
            BoolCommand bi bv ->
                do issue "h"
                   _ <- responses "name? "
                   issue (idString bi)
                   _ <- responses "state? "
                   issue (if bv then "1" else "0")
                   _ <- mainPrompt
                   System.IO.putStrLn (prettyPrintCommand cmd ++ "... ok")
                   autho' <- return (liftM (Authorize.setBool bi bv) autho)
                   execute autho' cmds
            AccessCommand subj obj cl ->
                do checkpolicyAv <-
                       do subjSido <- contextToSido subj
                          case subjSido of
                            Nothing -> return Symbol.emptySymSet
                            Just subjSid ->
                                do objSido <- contextToSido obj
                                   case objSido of
                                     Nothing -> return Symbol.emptySymSet
                                     Just objSid ->
                                         do issue "0"
                                            _ <- responses "source sid?  "
                                            issue subjSid
                                            _ <- responses "target sid?  "
                                            issue objSid
                                            _ <- responses "target class?  "
                                            issue (idString cl)
                                            liftM extractAv mainPrompt
                   System.IO.putStrLn
                       (prettyPrintCommand cmd ++ "... " ++
                        Symbol.prettyPrintSymSet checkpolicyAv)
                   System.IO.hFlush System.IO.stdout
                   case autho of
                     Nothing -> return ()
                     Just auth -> crossCheck auth cmd subj obj cl checkpolicyAv
                   execute autho cmds
            TransitionCommand subj obj new newCl ->
                let objCl = relatedObjectClass symbol newCl in
                do System.IO.putStrLn (prettyPrintCommand cmd ++ "... expanding")
                   System.IO.hFlush System.IO.stdout
                   execute autho (AccessCommand subj obj objCl :
                                  AccessCommand subj new newCl :
                                  AccessCommand new obj objCl : cmds)
            DefaultTransitionCommand subj obj new cl ->
                do subjSid <- contextToSid "subj" subj
                   objSid <- contextToSid "obj" obj
                   issue "3"
                   _ <- responses "source sid?  "
                   issue subjSid
                   _ <- responses "target sid?  "
                   issue objSid
                   _ <- responses "object class?  "
                   issue (idString cl)
                   newSid' <- liftM (extractSid "transition") mainPrompt
                   new' <- sidToContext "new" newSid'
                   if new' == new then return () else
                       error ("Computed wrong security context for\n" ++
                              prettyPrintCommand cmd ++
                              "\nstated security context: " ++
                              prettyPrintSecurityContext new ++
                              "\ncheckpolicy security context: " ++
                              prettyPrintSecurityContext new')
                   System.IO.putStrLn (prettyPrintCommand cmd ++ "... ok")
                   System.IO.hFlush System.IO.stdout
                   execute autho cmds

      contextToSid s sc =
          do sido <- contextToSido sc
             case sido of
               Just sid -> return sid
               Nothing -> error ("bad " ++ s ++ " security context: " ++
                                 prettyPrintSecurityContext sc)

      contextToSido (SecurityContext u r t) =
          do issue "2"
             _ <- responses "scontext?  "
             issue (idString u ++ ":" ++ idString r ++ ":" ++ idString t)
             liftM extractSido mainPrompt

      extractSid n s =
          case extractSido s of
            Just sid -> sid
            Nothing -> error ("bad " ++ n ++ " sid")

      extractSido =
          \s -> case ReadP.readP_to_S parse s of
                  [(sid,_)] -> sid
                  _ -> error ("couldn't parse SID response:\n" ++ show s)
          where
            parse = parseFirstParsableLine
                      ((do _ <- ReadP.string "libsepol.context_from_record: invalid security context: "
                           return Nothing) ReadP.+++
                       (do _ <- ReadP.string "sid "
                           sid <- ReadP.munch1 Char.isDigit
                           return (Just sid)))

      sidToContext s sid =
          do sco <- sidToContexto sid
             case sco of
               Just sc -> return sc
               Nothing -> error ("bad " ++ s ++ " sid: " ++ sid)

      sidToContexto sid =
          do issue "1"
             _ <- responses "sid?  "
             issue sid
             liftM extractSecurityContext mainPrompt

      extractSecurityContext =
          \s -> case ReadP.readP_to_S parse s of
                  [(sc,_)] -> sc
                  _ -> error ("couldn't parse security context response:\n" ++
                              show s)
          where
            parse = parseFirstParsableLine
                      ((do _ <- ReadP.string "libsepol.sepol_sid_to_context: unrecognized SID "
                           return Nothing) ReadP.+++
                       (do _ <- ReadP.string "scontext "
                           sc <- parseSecurityContext
                           return (Just sc)))

      extractAv =
          \s -> case ReadP.readP_to_S parse s of
                  [(allow,_)] -> allow
                  x -> error ("couldn't parse ACCESS response:\n" ++ show x)
          where
            parse = parseFirstParsableLine
                      (do _ <- ReadP.string "allowed "
                          ps <- parseSet parseIdentifier
                          case runP (Symbol.fromListSymSet ps) of
                            Left err -> error ("error in checkpolicy " ++
                                               "permission set:\n" ++ err)
                            Right perm -> return perm)

      crossCheck auth cmd subj obj cl checkpolicyAv =
          do computedAv <-
                 case Authorize.accessVector symbol auth subj obj cl of
                   Left err ->
                       error ("Error computing access vector for " ++
                              show subj ++ " " ++ show obj ++ " " ++
                              show cl ++ ":\n" ++ err)
                   Right av -> return av
             if checkpolicyAv == computedAv then return () else
                 error ("Computed wrong access vector for\n" ++
                        prettyPrintCommand cmd ++
                        "\ncheckpolicy AV: " ++
                        Symbol.prettyPrintSymSet checkpolicyAv ++
                        ":\ncomputed AV: " ++
                        Symbol.prettyPrintSymSet computedAv)

genCheckpolicyCommand :: Bool -> String -> IO [String]
genCheckpolicyCommand interactive filename = do
       policyProgram <-
           getEnv "CHECKPOLICY" `catch`
           const (return "checkpolicy/checkpolicy")
       rootDir <-
           getEnv "SCDROOTDIR" `catch`
           const (return "dev/scd/SCD")
       plat <- platform
       let command ssh = policyProgram : (if interactive then ["-d"] else []) ++
                         [(if ssh then rootDir ++ "/" else "") ++ filename]
       command' <- if plat == "Linux" then return (command False)
                   else return ("ssh" : (if interactive then ["-t"] else []) ++
                                ["umbra"] ++ command True)
       return (if not interactive then command' else
                   ["expect", "-c"] ++
                   ["spawn " ++ concat (List.intersperse " " command') ++
                    " ; interact"])

crossCheckpolicyCommand :: String -> IO [String]
crossCheckpolicyCommand = genCheckpolicyCommand True

badCheckpolicyCommand :: String -> IO [String]
badCheckpolicyCommand = genCheckpolicyCommand False

crossCheckpolicy ::
    String -> Maybe (Symbol.SymbolTable, Authorize.Authorize) ->  IO ()
crossCheckpolicy pathFilename symbol_auth =
    do commands <- readCommands pathFilename
       checkpolicy <- crossCheckpolicyCommand pathFilename
--       putStrLn (concat (List.intersperse " " (map quotify checkpolicy)))
       (inp,out,_,_) <-
           System.Process.runInteractiveProcess
             (head checkpolicy) (tail checkpolicy) Nothing Nothing
       let issue cmd =
               do System.IO.hPutStrLn inp cmd
                  System.IO.hFlush inp
{-
                  System.IO.putStrLn cmd
                  System.IO.hFlush System.IO.stdout
-}
                  return ()
       let responses = waitForPrompt out
       let (symbol,autho) =
               case symbol_auth of
                 Nothing -> (undefined,Nothing)
                 Just (s,a) -> (s, Just a)
       executeCommands issue responses symbol autho commands

badCheckpolicy :: String -> IO ()
badCheckpolicy pathFilename =
    do checkpolicy <- badCheckpolicyCommand pathFilename
{-
       putStrLn (concat (List.intersperse " " (map quotify checkpolicy)))
-}
       (_,_,err,_) <-
           System.Process.runInteractiveProcess
             (head checkpolicy) (tail checkpolicy) Nothing Nothing
       output <- waitForDfa err "checkpolicy error" errorDfa
       putStrLn output
       return ()

    where
      errorDfa = stringToDfa "error(s) encountered while parsing configuration"
         `orDfa` stringToDfa "assertion violations occured"
         `orDfa` stringToDfa "Error while expanding policy"

testDirectory :: String
testDirectory = "testsrc/SCD/SELinux/Test/data"

confSuffix :: String
confSuffix = ".conf"

findPrefixPolicyFiles :: String -> String -> IO [String]
findPrefixPolicyFiles dir prefix =
    do files <- System.Directory.getDirectoryContents dir
       indexes <- return (Maybe.mapMaybe fileToIndex files)
       return (map indexToFile (List.sort indexes))
    where
      indexToFile n = prefix ++ show n ++ confSuffix

      fileToIndex s =
          if not (List.isPrefixOf prefix s) then Nothing else
              case reads (List.drop (length prefix) s) of
                [(n,s')] -> if s' == confSuffix then Just (n :: Int) else Nothing
                _ -> Nothing

-- checkTestPolicy :: String -> IO ()
-- checkTestPolicy filename =
--     let pathFilename = testDirectory ++ "/" ++ filename in
--     do putStrLn ("Checking test policy file " ++ filename)
--        (_,symbol,auth) <- buildAuthorizeFile pathFilename
--        putStr ("built the authorization relation for " ++
--                filename ++ ":\n" ++ Authorize.summarize auth)
--        crossCheckpolicy pathFilename (Just (symbol,auth))
--        return ()

checkAmbiguousPolicy :: String -> IO ()
checkAmbiguousPolicy filename =
    let pathFilename = testDirectory ++ "/" ++ filename in
    do putStrLn ("Checking ambiguous policy file " ++ filename)
       buildResult <- tryBuildAuthorizeFile pathFilename
       case buildResult of
         Left err -> putStrLn err
         Right _ -> error "Failed to detect an ambiguous policy file"
       crossCheckpolicy pathFilename Nothing
       return ()

checkBadPolicy :: String -> IO ()
checkBadPolicy filename =
    let pathFilename = testDirectory ++ "/" ++ filename in
    do putStrLn ("Checking bad policy file " ++ filename)
       buildResult <- tryBuildAuthorizeFile pathFilename
       case buildResult of
         Left err -> putStrLn err
         Right _ -> error "Failed to detect a bad policy file"
       badCheckpolicy pathFilename
       return ()

checkAllPolicies :: IO ()
checkAllPolicies =
    do -- testPolicies <- findPrefixPolicyFiles testDirectory "test"
       -- mapM_ checkTestPolicy testPolicies
       ambiguousPolicies <- findPrefixPolicyFiles testDirectory "ambiguous"
       mapM_ checkAmbiguousPolicy ambiguousPolicies
       badPolicies <- findPrefixPolicyFiles testDirectory "bad"
       mapM_ checkBadPolicy badPolicies

authorizeTest :: String -> Assertion
authorizeTest filename = do
  policy <- SELinuxParser.parsePolicyFile filename
  let result = do
        (policy', symbols) <- left (showString "Could not initiate authorizeTest: ") $
                              Symbol.tryBuildSymbolTable policy
        Authorize.build policy' symbols
        return ()
  either assertFailure return result

testPolicies :: IO Test
testPolicies =
    do t <- (fmap . map) (\(n, a)->buildTest n a) assertions
       return $ testGroup "Check Test Policies" t
    where
      assertions :: IO [(String, Assertion)]
      assertions = (fmap . map) (\name->(name, buildAssertion name)) testPolicies

      testPolicies :: IO [String]
      testPolicies = findPrefixPolicyFiles testDirectory "test"

      buildTest :: String -> Assertion -> Test
      buildTest filename assertion = testCase ("Check file: " ++ filename) assertion

      buildAssertion :: String -> Assertion
      buildAssertion filename =
              let pathFilename = (testDirectory ++ "/" ++ filename) in
              do
                 tryResult <- tryBuildAuthorizeFile pathFilename
                 case tryResult of
                   Left err              -> assertFailure err
                   Right (_,symbol,auth) -> crossCheckpolicy pathFilename (Just (symbol,auth))

testCases :: String -> IO [Test]
testCases filename = do pTests <- testPolicies
                        return [ testGroup "Check All Policies"
                                               [ pTests
                 -- ambiguousPolicies <- findPrefixPolicyFiles testDirectory "ambiguous"
                 -- mapM_ checkAmbiguousPolicy ambiguousPolicies
                 -- badPolicies <- findPrefixPolicyFiles testDirectory "bad"
                 -- mapM_ checkBadPolicy badPolicies
                                               ]
                               , testCase ("Test authorization relation for "++filename) $
                                          authorizeTest filename
                               ]

-- checks :: String -> Policy -> Symbol.SymbolTable -> IO Authorize.Authorize
-- checks filename policy symbol =
--     do putStrLn "\nBegin tests of the SELinux policy authorization relation"
--        checkAllPolicies
--        auth <- authorizeTest filename policy symbol
--        putStrLn "End tests of the SELinux policy authorization relation"
--        return auth
