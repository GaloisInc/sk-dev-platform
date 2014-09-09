
-- | Generate Lobster prelude from Flask information (classes and permissions)

import System.Exit(exitFailure)
import System.Environment(getProgName, getArgs)
import System.IO(hPutStrLn, stderr)

import SCD.SELinux.Syntax(idString)
import SCD.M4.Options(defaultOptions)
import SCD.M4.KindCheck(defaultKindCheckOptions)
import SCD.M4.ModuleFiles(readPolicy)
import SCD.M4.KindCheck(kcBaseEnv, ClassPermissionMap)
import SCD.Lobster.Syntax.Abs(Statement(..), PortDeclarationType(..),
                              PortDeclarationConnection(..),)
import qualified SCD.Lobster.Syntax.Abs as L
import SCD.Lobster.Syntax.Print(printTree)
import SCD.Lobster.Syntax(mkId, typeClass)
import SCD.Lobster.Policy(typeClassPermissionPort)
import SCD.Lobster.Utilities(capitalize)

import Data.Foldable(toList)
import Data.Map(assocs)


usage :: IO ()
usage = do
  n <- getProgName
  hPutStrLn stderr $ "Usage:\n"++n++" <policy directory>"
  exitFailure

main :: IO ()
main = do
  l <- getArgs
  case l of [d] -> generate d
            _   -> usage

generate :: FilePath -> IO ()
generate d = do
  p <- readPolicy Nothing d
  let ((clm,_),_errs) = kcBaseEnv defaultOptions defaultKindCheckOptions p
  putStr (printTree (genPrimitiveClasses clm++[genTypeClass clm]))

genTypeClass :: ClassPermissionMap -> Statement
genTypeClass clm = ClassDeclaration typeClass [ti] (concatMap port (assocs clm))
  where port (c,ps) = [ PortDeclaration (typeClassPermissionPort c p)
                                        EmptyPDT EmptyPDC
                      | p <- toList ps ]
        ti = mkId "selinuxType"

genPrimitiveClasses :: ClassPermissionMap -> [Statement]
genPrimitiveClasses clm =
  [ ClassDeclaration (classify c) [] (map portify (toList ps) )
  | (c,ps) <- assocs clm ]
  where classify = mkId . capitalize . idString
        portify p = PortDeclaration (mkId (idString p)) EmptyPDT EmptyPDC
