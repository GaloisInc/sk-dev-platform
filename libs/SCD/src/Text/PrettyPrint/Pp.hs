{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Text.PrettyPrint.Pp(Pp(..), above, pnest, pbraces, pbracesH,
  star, minus, tilde, period, sepWithCommas, squote, backquote, slash,
  dquote, underline, questionmark, dollar, render) where

import Text.PrettyPrint.HughesPJ(Doc, text, empty, nest, lbrace,
  rbrace, ($+$), (<+>), sep, punctuate, comma, render)

import Data.Foldable(Foldable, toList)

class Pp a where
  pp :: a -> Doc

instance Pp Doc where
  pp = id

above :: (Pp a, Foldable l) => l a -> Doc
above = foldr ($+$) empty . fmap pp . toList

pnest :: Doc -> Doc
pnest = nest 4

pbraces :: (Pp a, Foldable l) => l a -> Doc
pbraces d = lbrace $+$ pnest (above d) $+$ rbrace

pbracesH :: Doc -> Doc
pbracesH d = lbrace <+> d <+> rbrace

sepWithCommas :: (Pp a, Foldable l) => l a -> Doc
sepWithCommas l = sep (punctuate comma (map pp (toList l)))

star :: Doc
star = text "*"

minus :: Doc
minus = text "-"

tilde :: Doc
tilde = text "~"

period :: Doc
period = text "."

squote :: Doc
squote = text "'"

dquote :: Doc
dquote = text "\""

backquote :: Doc
backquote = text "`"

slash :: Doc
slash = text "/"

underline :: Doc
underline = text "_"

questionmark :: Doc
questionmark = text "?"

dollar :: Doc
dollar = text "$"
