module Data.Strict(hyperStrict, returnHyperStrict) where

hyperStrict :: Eq a => a -> b -> b
hyperStrict a b = a==a `seq` b

returnHyperStrict :: (Eq a, Monad m) => a -> m a
returnHyperStrict a = hyperStrict a (return a)
