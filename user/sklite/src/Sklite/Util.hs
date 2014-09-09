module Sklite.Util
    ( replace
    )
where

import Control.Applicative

replace :: (Eq a) => a -> [a] -> [a] -> [a]
replace from to cs = concat $ (\c -> if c == from then to else [c]) <$> cs