{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SCD.Lobster.Symbolic.Lobster
    ( LobsterAST       -- Generated Lobster AST type
    , readFile         -- Generate AST for a readFile
    , writeFile        -- Generate AST for a writeFile
    , putStrLn         -- Generate AST for a putStrLn
    , system           -- Generate AST for starting program
    , rawSystem        -- Generate AST for starting program
    , runGenerate
    , runExecute
    , lift
    , M
    ) where

import Data.Symbolic.IO          ( SIO, sio )
import qualified Data.Symbolic.IO as SIO
import qualified Control.Monad.Trans as Trans
import Control.Monad.State(StateT, MonadState, runStateT, get, put)
import Control.Monad.Reader(ReaderT, MonadReader, runReaderT, asks)
import SCD.Lobster.Symbolic.AstUtil ( mkFileInstance, (<==), (==>), (===)
                                    , mkProcInstance
                                    )
import Lobster.Abs(Statement, Policy(..))
import Prelude hiding            ( readFile, putStrLn, writeFile )
import qualified Prelude
import qualified System.Cmd as Cmd
import System.Exit(ExitCode)
import System.FilePath((</>))
import Data.Char(isSpace, toLower)
import SCD.Lobster.Symbolic.LobsterAST  ( LobsterAST (..), justStmts
                                        , getStatements )
import SCD.Lobster.Symbolic.LobsterUtil ( active, child, getattr, readPort
                                        , writePort , createPort )

defaultInstallPath :: FilePath
defaultInstallPath = "/usr/local/bin"

-- Generate AST for a single process implemented as a Haskell program.
runGenerate :: FilePath -> M a -> IO LobsterAST
runGenerate path prog = do
  let prog' = do p <- currentProcessName
                 lift $ sio ( return () , justStmts 
                                            ([ mkProcInstance p 
                                                (defaultInstallPath </> path) ]
                                             ++ environmentConnections 
                                                 p environmentInstance))
                 prog
  let first _ [] = []               
      first f (x:xs) = (f x:xs)
  p <- SIO.runGenerate (run (first toLower path) prog')
  return LobsterAST{ moniker = moniker p
                   , policy = Policy ( getStatements p )
                   }

environmentConnections :: String -> String -> [Statement]
environmentConnections a env = [ (a,e) === (env,process_port_prefix++e) 
                               | e <- environmentPorts ]
  where process_port_prefix = "process_"

environmentInstance :: String
environmentInstance = "environment"

environmentPorts :: [String]
environmentPorts = words "active exec_active create"

runExecute :: M a -> IO a
runExecute = SIO.runExecute . run ""

newtype M a = M ( ReaderT E (StateT S (SIO LobsterAST IO)) a)
  deriving (Monad, MonadReader E, MonadState S)

lift :: SIO LobsterAST IO a -> M a
lift m = M (Trans.lift (Trans.lift m))

run :: String -> M a -> SIO LobsterAST IO a
run p (M m) = fst `fmap` runStateT (runReaderT m E{ prefix = p }) S{ fresh = 0 }

data E = E{ prefix :: String }
data S = S{ fresh :: Integer }

gensym :: String -> M String
gensym p = do
  s <- get
  let n = fresh s
  put s{ fresh = n + 1 }
  pre <- asks prefix
  return (pre++"_"++p++show n) 

currentProcessName :: M String
currentProcessName = do
  pre <- asks prefix 
  return (pre++"_p")

--------------------------------------------------------------------------------
-- Operations and data types that form the API
--------------------------------------------------------------------------------

-- For a readFile, we will generate the same as for an openFile, with the
-- addition of an access to the read port of the file. This is because a
-- readFile does an open for you, and then does the read also.
-- The Lobster code we are generating AST for is:
--    domain f = File ( path );
--    p.active --> f.getattr;
--    p.active --> f.read;
readFile :: FilePath -> M String
readFile f =
  do d <- currentProcessName
     p <- gensym "d"
     lift $ sio ( Prelude.readFile f
                , justStmts [ mkFileInstance p f
                            , active d <== getattr p 
                            , active d <== readPort p
                            ]
                )

writeFile :: FilePath -> String -> M ()
writeFile f s =
  do d <- currentProcessName
     p <- gensym "d"
     lift $ sio ( Prelude.writeFile f s
                , justStmts [ mkFileInstance p f
                            , active d <== getattr p
                            , active d ==> writePort p
                            ]
                )

putStrLn :: String -> M ()
putStrLn s =
  let f = "/dev/stdout"
  in do d <- currentProcessName
        p <- gensym "d"
        lift $ sio ( Prelude.putStrLn s
                   , justStmts [ mkFileInstance p f
                               , active d <== getattr p 
                               , active d ==> writePort p
                               ]
                   )

system :: String -> M ExitCode
system cmd = systemCommand cmd (Cmd.system cmd)

rawSystem :: String -> [String] -> M ExitCode
rawSystem cmd args = systemCommand cmd (Cmd.rawSystem cmd args)

systemCommand :: String -> IO ExitCode -> M ExitCode
systemCommand cmd io = do
  d <- currentProcessName
  p <- gensym "p"
  lift $ sio (  io
             , justStmts [ mkProcInstance p (takeWhile (not . isSpace) cmd)
                         , active d ==> createPort p
                         , active p === child d 
                         ]
             )
