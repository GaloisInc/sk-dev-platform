{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  CSD.Lobster.Gen.CoreSyn.Output
Description :  Outputting simple/core Lobster in its concrete form.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  The SCD team
Stability   :  provisional
Portability :  portable

The pretty printer for "SCD.Lobster.Gen.CoreSyn", the
output form being Lobster concrete syntax.
-}
module SCD.Lobster.Gen.CoreSyn.Output where

import SCD.Lobster.Gen.CoreSyn
import Text.PrettyPrint
import Data.List

showLobster :: [Decl] -> String
showLobster ds = render $ ppLobster ds

ppLobster :: [Decl] -> Doc
ppLobster ds = vcat (map (ppDecl 0) ds)

iBody :: Int
iBody = 3

ppDecl :: Int -> Decl -> Doc
ppDecl i d =
 case d of
   Class nm ps body ->
    nest i $
     text "class" <+>
      ppName nm <+>
       parens (hcat (intersperse (text ", ") (map ppName ps))) <+> char '{' $$
        vcat (map (ppDecl iBody) body) $$
       text "}" $$
       text ""
   Port nm pcs (di,ps) ->
     nest i $
      text "port" <+> ppName nm <+>
       (if null pcs then empty
        else text ":" <+> braced (intersperse (text ", ") (map ppPC pcs))) <>
       (if null ps then empty else space <> ppDir True di <+> hsep (map ppDP ps)) <>
         char ';'
   Domain v f args ->
     nest i $
      text "domain" <+> ppName v <+> char '=' <+>
        ppName f <> parens (hcat (intersperse (text ", ") (map ppName args))) <> char ';'
   Type t as ->
     nest i (
      text "type" <+> ppName t <> char ';' <+>
        (if null as
	  then empty
	  else (text "// attributes:" <+> hsep (map ppName as))))

   Connect dpA dpB di ->
     nest i $
      ppDP dpA <+> ppDir True di <+> ppDP dpB <> char ';'

   Comment "" -> text "" -- hack!
   Comment s -> nest i $ text ("// " ++ s)

braced :: [Doc] -> Doc
braced [] = empty
braced xs = braces (hcat xs)

ppPC :: PortConstraint -> Doc
ppPC pc =
 case pc of
   PortDir d   -> text "direction=" <> ppDir False d
   PortPos isS -> text "position=" <> text (if isS then "subject" else "object")
   PortType t  -> text "type=" <> ppName t

ppName :: Name -> Doc
ppName (Name n) = text n

ppDir :: Bool -> Dir -> Doc
ppDir asArrow d = text $
  case d of
    L | asArrow   -> "<--"
      | otherwise -> "input"
    R | asArrow   -> "-->"
      | otherwise -> "output"
    N | asArrow   -> "--"
      | otherwise -> "bidirectional"
    B | asArrow   -> "<-->"
      | otherwise -> "bidirectional"

ppDP :: DomPort -> Doc
ppDP d = ppName (portDomain d) <> char '.' <> ppName (portLabel d)
