{-# OPTIONS_GHC -Wall -Werror #-}
module Main(main) where

import System.IO
import Text.PrettyPrint.Pp(
  render,
  above,
  pp)

import qualified Lobster.SELinux.SELinux as SELinux
import Lobster.Common

main :: IO ()
main = do
  (options,fns) <- processOptions
  domain <- parseAndInterpretPolicyFiles_ options fns
  let output = outputFile options
  let selinux = SELinux.compileDomain output domain
  System.IO.writeFile (output ++ ".te")
    (render (pp (SELinux.typeEnforcement selinux))++"\n")
  System.IO.writeFile (output ++ ".fc")
    (render (above (map pp (SELinux.fileContext selinux)))++"\n")
