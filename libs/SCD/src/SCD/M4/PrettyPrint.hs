{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans -cpp #-}
{- |
Module      : $Header$
Description : SELinux M4 pretty printing
Copyright   : (c) Galois, Inc.

SELinux M4 pretty printing
-}

module SCD.M4.PrettyPrint(pp, interfaceTypeString) where

import SCD.M4.Syntax(Interface(..) , ModuleDoc(..),
  InterfaceElement(..), InterfaceType(..), InterfaceDoc(..),
  Parameter(..), InfoFlow(..), Flow(..), Stmts, Require(..), Stmt(..),
  GenContext(..), M4Var(..), M4Id(..), IfdefId(..), LayerId(..), ModuleId(..),
  MlsRange(..), LevelId(..), XMLDoc(..), RefPolicyWarnLine(..),
  Implementation(..), Version(..), FileContexts(..), FileContext(..),
  HomePath(..), RegexpPath(..), BoolType(..))

import SCD.SELinux.Syntax(idString)

import SCD.SELinux.PrettyPrint(ppAlias, ppOptAttrs, ppMaybeFileType)

import Data.Foldable(Foldable)

import Text.PrettyPrint.HughesPJ(Doc, text, empty, ($+$), (<+>), (<>),
  semi, parens, colon, comma, render)

import Text.PrettyPrint.Pp(Pp(..), above, pbraces, sepWithCommas,
  squote, backquote, minus, slash)

import Text.XML.Light(unode, Attr(..), unqual, Content(Elem), ppContent, Element)

instance Pp Interface where
  pp (InterfaceModule ds mes) = 
        pp ds
    $+$ text ""
    $+$ above mes

instance Pp ModuleDoc where
  pp (ModuleDoc ms md r rd) = 
    above $
         [ unode "summary" ms ]
        ++ nodeDesc md
        ++ (if r then [ unode "required"
                             ( Attr (unqual "val")
                                    (render (pp r))
                             , rd )
                      ]
                 else [])

instance Pp InterfaceDoc where
  pp (InterfaceDoc is d ps f) =
    above $
         [ unode "summary" is ]
        ++ nodeDesc d
        ++ concatMap nodeParameter ps
        ++ nodeInfoFlow f

nodeDesc :: [Content] -> [Element]
nodeDesc [] = []
nodeDesc md = [ unode "desc" md ]

nodeParameter :: Parameter -> [Element]
nodeParameter (Parameter _l n ps o) =
  [ unode "param" ([ Attr (unqual "name") n ]
                 ++ if o then [Attr (unqual "optional") "true"]
                         else [])
  , unode "summary" ps
  ]

instance Pp Parameter where
  pp = above . nodeParameter

nodeInfoFlow :: Maybe InfoFlow -> [Element]
nodeInfoFlow Nothing = []
nodeInfoFlow (Just (InfoFlow f w)) =
  [ unode "infoflow"
         ([ Attr (unqual "type") (render (pp f)) ]
          ++ if f == None then [] else
             [ Attr (unqual "weight") (show w) ])
  ]

instance Pp Element where
  pp = above . map (text . ("## "++)) . lines . ppContent . Elem

squotes :: Pp a => a -> Doc
squotes a = backquote <> pp a <> squote

ppM4Call :: Pp a => String -> [a] -> Doc
ppM4Call k as = text k <> parens (sepWithCommas (map squotes as))

instance Pp Flow where
  pp Read  = text "read"
  pp Write = text "write"
  pp None  = text "none"
  pp Both  = text "both"

instance Pp InterfaceElement where
  pp (InterfaceElement it ds i ss) = 
   pp ds $+$ ppM4Call (interfaceTypeString it) [pp i, above ss]

interfaceTypeString :: InterfaceType -> String
interfaceTypeString InterfaceType = "interface"
interfaceTypeString TemplateType  = "template"

instance Pp XMLDoc where
  pp (XMLDoc ds) = above (map text (lines ds)) $+$ text ""


ppIfdef :: (Pp a, Foldable l) => IfdefId -> l a -> [a] -> Doc
ppIfdef i t e = ppM4Call "ifdef" ([pp i,above t]++ 
                                  if null e then [] else [above e])

instance Pp Require where
  pp (RequireClass i ps)  = text "class" <+> pp i <+> pp ps <> semi
  pp (RequireRole      l) = text "role"      <+> sepWithCommas l <> semi
  pp (RequireType      l) = text "type"      <+> sepWithCommas l <> semi
  pp (RequireAttribute l) = text "attribute" <+> sepWithCommas l <> semi
  pp (RequireBool      l) = text "bool"      <+> sepWithCommas l <> semi
  pp (RequireIfdef i t e) = ppIfdef i t e
  pp (RequireIfndef i t)  = ppM4Call "ifndef" ([pp i,above t])

emptyStmts :: Stmts -> Bool
emptyStmts ss = null ss

instance Pp Stmt where
  pp (Tunable c s1 s2) = ppM4Call "tunable_policy" 
                         ([pp c, above s1] ++
                          if emptyStmts s2 then [] else [above s2])
  pp (Optional ss s2) = ppM4Call "optional_policy" 
                       ([above ss] ++
                        if emptyStmts s2 then [] else [above s2])
  pp (Ifdef i s1 s2) = ppIfdef i s1 s2
  pp (Ifndef i s1) = ppM4Call "ifndef" ([pp i, above s1])
  pp (RefPolicyWarn (RefPolicyWarnLine s)) = ppM4Call "refpolicywarn" [text s]
  pp (Call i is) = text (idString i) <> parens (sepWithCommas is)
  pp (Role r []) = text "role" <+> pp r <> semi
  pp (Role r ts) = text "role" <+> pp r <+> text "types" <+> pp ts <> semi
  pp (RoleTransition rs ts r) = text "role_transition" <+> 
                                pp rs <+> pp ts <+> pp r <> semi
  pp (RoleAllow r1 r2) = text "allow" <+> pp r1 <+> pp r2 <> semi
  pp (Attribute i) = text "attribute" <+> pp i <> semi
  pp (Type t als ats) = text "type" <+> pp t <+> ppAlias als <> 
                        ppOptAttrs ats <> semi
  pp (TypeAlias t a) = text "typealias" <+> pp t <+> ppAlias a <> semi
  pp (TypeAttribute t l) = text "typeattribute" <+> pp t <+> 
                           sepWithCommas l <> semi
  pp (RangeTransition t1 t2 cs l) = text "range_transition" <+> pp t1 <+>
                                    pp t2 <> colon <> 
                                    pp cs <+> pp l <> semi
  pp (TeNeverAllow st ps) = text "neverallow" <+> pp st <+> pp ps <> semi
  pp (Transition tr st t) = pp tr <+> pp st <+> pp t <> semi
  pp (TeAvTab a st ps) = pp a <+> pp st <+> pp ps <> semi
  pp (CondStmt c s1 s2) = text "if" <+> parens (pp c) $+$ pbraces s1
                      $+$ if emptyStmts s2 then empty 
                          else text "else" $+$ pbraces s2
  pp (XMLDocStmt d) = pp d
  pp (SidStmt s) = pp s
  pp (FileSystemUseStmt s) = pp s
  pp (GenFileSystemStmt s) = pp s
  pp (PortStmt s) = pp s
  pp (NetInterfaceStmt s) = pp s
  pp (NodeStmt s) = pp s
  pp (Define i) = ppM4Call "define" [i]
  pp (Require rs) = ppM4Call "gen_require" [above rs]
  pp (GenBoolean t i b) = pp t <> parens (sepWithCommas [pp i, pp b])

instance Pp BoolType where
  pp BoolType    = text "gen_bool"
  pp TunableType = text "gen_tunable"

instance Pp GenContext where
  pp (GenContext u r t l) = 
    text "gen_context" <> parens (pp u <> colon <> pp r <> colon <> pp t <> comma <> pp l)

instance Pp MlsRange where
  pp (MlsRange r1 r2)  | r1 == r2  = pp r1
                       | otherwise = pp r1 <+> minus <+> pp r2

instance Pp Implementation where
  pp (Implementation i v s) = text "policy_module" <> parens (pp i <> comma <> pp v)
                          $+$ above s

instance Pp Version where
  pp (Version s) = text s

instance Pp FileContexts where
  pp (FileContexts fc) = above fc

instance Pp FileContext where
  pp (FileContext p mt mg) = pp p <+> ppMaybeFileType mt <+> ppMaybeGenContext mg
  pp (FileContextIfdef i s1 s2) = ppIfdef i s1 s2
  pp (FileContextIfndef i s1) = ppM4Call "ifndef" ([pp i, above s1])

ppMaybeGenContext :: Maybe GenContext -> Doc
ppMaybeGenContext = maybe (text "<<none>>") pp

instance Pp HomePath where
  pp (HomeDir r) = text "HOME_DIR" <> pp r
  pp (HomeRoot r) = text "HOME_ROOT" <> pp r
  pp (Path r)     = pp r

instance Pp RegexpPath where
  pp (PlainPath fp) = pp fp
  pp (RegexpPath s) = text s

instance Pp (LayerId,ModuleId) where
  pp (l,m) = pp l <> slash <> pp m

#ifndef __HADDOCK__
deriving instance Pp M4Var
deriving instance Pp M4Id
deriving instance Pp IfdefId
deriving instance Pp LevelId
deriving instance Pp LayerId
deriving instance Pp ModuleId
#endif
