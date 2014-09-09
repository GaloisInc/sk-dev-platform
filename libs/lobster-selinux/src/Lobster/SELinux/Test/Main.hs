{-# OPTIONS_GHC -Wall -Werror #-}

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.Directory(getDirectoryContents)
import System.FilePath((</>))

import qualified Lobster.SELinux.Test.SELinux as SELinux

testappDirectory :: String
testappDirectory = "../SELinux/testapp"

lobsterExamplePoliciesDirectory :: String
lobsterExamplePoliciesDirectory = "test/examples"

testappPolicy :: String
testappPolicy = testappDirectory </> "testapp.lsr"

getLobsterExamplePolicies :: IO [(Bool,FilePath)]
getLobsterExamplePolicies = do
    fns <- getDirectoryContents lobsterExamplePoliciesDirectory
    return (map rejoin $ List.sort $ Maybe.mapMaybe split fns)

  where
    split :: String -> Maybe (String,Int,String,String)
    split f =
        let (v,f') = List.span Char.isAlpha f in
        let (n,f'') = List.span Char.isDigit f' in
        if f'' == ".lsr" then Just (v, length n, n, f) else Nothing

    rejoin :: (String,Int,String,String) -> (Bool,String)
    rejoin (v,_,_,f) =
        let b = if v == "example" then False
                else if v == "error" then True
                else error $ "bad test file prefix: " ++ v in
        (b, lobsterExamplePoliciesDirectory </> f)

getLobsterPolicies :: IO [(Bool,FilePath)]
getLobsterPolicies = do
    fns <- getLobsterExamplePolicies
    return $ fns ++ [(False,testappPolicy)]

main :: IO ()
main =
    do lobsterPolicies <- getLobsterPolicies
       SELinux.checks lobsterPolicies
