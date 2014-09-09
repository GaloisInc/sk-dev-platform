{- |
Module      : $Header$
Description : Entry point for kind-checking SELinux policies.
Copyright   : (c) Galois, Inc.

This module provides an entry point for parsing and subsequently
kind-checking an SELinux policy directory.  It currently supports error
output in human-readable, textual form, or in XML.
-}

module SCD.M4.KindCheckPolicy(kcPolicyDoc, errorDoc, xmlErrorDoc) where

import SCD.M4.Options(Options(..))
import SCD.M4.KindCheck(KindCheckOptions(..), kcPolicy)
import SCD.M4.Errors(Error, occurrences, flatten, errorsByType)
import SCD.M4.ErrorsXml(genXmlOutput)
import SCD.M4.ModuleFiles(readPolicy, readPolicyModule)
import Text.PrettyPrint.HughesPJ(Doc, empty, text, ($+$))
import Text.PrettyPrint.Pp(above, pp, pnest)
import Text.XML.Light(Element)
import Data.List(sort, intersperse)
import System.Directory(canonicalizePath)

-- | Parses and kind-checks the given SELinux policy directory.  Options
-- can control many aspects of the kind-checking behavior (e.g., XML
-- vs. textual error output, etc).
kcPolicyDoc :: Options -> KindCheckOptions -> FilePath -> [FilePath] -> IO (Doc, Maybe Element)
kcPolicyDoc os kos pd pmfs = do
  pd':pmfs' <- if xmlErrorOutput os then mapM canonicalizePath (pd:pmfs)
                                   else return (pd:pmfs)
  pms <- mapM readPolicyModule pmfs'
  p <- readPolicy (ifdefDeclFile os) pd'
  let (_ke, errs) = kcPolicy os kos p pms
  let edoc = if xmlErrorOutput os then xmlErrorDoc else errorDoc
  return $ edoc os errs

-- | Generates textual error output for the given shrimp errors.
errorDoc :: Options -> [Error] -> (Doc, Maybe Element)
errorDoc os errs =
    (text "*** Statistics:" $+$
     above (intersperse (text "") (
         [ pp e $+$ if l > 1 then pnest (text ("("++show l++" errors like this.)")) else empty
         | (l,e) <- reverse (sort (occurrences (flatten errs)))])) $+$
     text "" $+$
     text "*** Complete error list:" $+$
     above (intersperse (text "") (if group os then concat (intersperse [text "--"]
                                                            (map (map pp) (errorsByType errs)))
                                   else map pp errs)),
     Nothing)                                   

-- | Generates XML error output for the given shrimp errors.  Note that
-- the XML parse tree will be Nothing when there are no errors present.
xmlErrorDoc :: Options -> [Error] -> (Doc, Maybe Element)
xmlErrorDoc _os errs = (text t, e) where (t,e) = genXmlOutput errs
