{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Lens
import Data.Data.Lens (tinplate)
import Data.List (find)
import Data.Set (Set)
import System.Environment
import System.Exit
import System.IO
import qualified Data.Set as Set

import SCD.SELinux.Parser
import qualified SCD.SELinux.Syntax as S

data Entity
  = ERole !S.RoleId
  | EType !S.TypeId
  | EAttribute !S.AttributeId
  deriving (Eq, Ord, Show, Read)

data Mode = UseRequired | UseOptional

makePrisms ''Entity
makePrisms ''S.TeRbac
makePrisms ''S.AvRule
makePrisms ''S.Require
makeLensesFor [("moduleDef","moduleDef"), ("moduleAvRules","moduleAvRules")] ''S.Module
makeLensesFor [("teRbacs","teRbacs")] ''S.Policy

loadArgs :: IO (Mode, FilePath, FilePath, [FilePath])
loadArgs = do
  xs <- getArgs
  case xs of
    "r" : rootTypesFn : baseFn : policyFns -> return (UseRequired, rootTypesFn, baseFn, policyFns)
    "o" : rootTypesFn : baseFn : policyFns -> return (UseOptional, rootTypesFn, baseFn, policyFns)
    _ -> do n <- getProgName
            hPutStrLn stderr ("Usage: " ++ n ++ " (r|o) TYPES_FILE BASE_FILE MODULE_FILE*")
            exitFailure

main :: IO ()
main = do

  let loadRootTypes = fmap lines . readFile

  (mode, rootTypesFn, baseFn, policyFns) <- loadArgs

  rootTypes  <- loadRootTypes       rootTypesFn
  basePolicy <- loadPolicy          baseFn
  policies   <- traverse loadModule policyFns

  let requirementEntities :: Fold S.TeRbac Entity
      requirementEntities f (S.Type      t1 t2 _) = coerce (folded f (map EType (t1 : t2)))
      requirementEntities f (S.Role      r _    ) = coerce (f (ERole r))
      requirementEntities f (S.Attribute a      ) = coerce (f (EAttribute a))
      requirementEntities f x                     = ignored f x

  let baseSet = Set.fromList (basePolicy ^.. teRbacs . folded . requirementEntities)

  let modules = search
                  policies
                  mode
                  (map (EType . S.mkId) rootTypes)   -- type queue
                  baseSet

  -- Render out modules as a list of policy filenames
  putStr . unlines
         . map S.idString
         . Set.toList
         $ modules

search ::
  [S.Module] {- ^ all policies -} ->
  Mode       {- ^ include optional sections -} ->
  [Entity]   {- ^ undefined entities -} ->
  Set Entity {- ^ defined entities  -} ->
  Set S.ModuleId {- ^ files used -}
search pols mode = go Set.empty
  where
  -- collect the required entities from an avrule based on the useOptional setting
  selectedReferences :: Fold S.AvRule S.Require
  selectedReferences = case mode of
    UseOptional -> allAvRuleReferences
    UseRequired -> requiredAvRuleReferences

  -- collect the required entities from a module
  entities :: Fold S.Module Entity
  entities = moduleAvRules . avRules . folded . selectedReferences . wrappedRequirements

  -- worker loop
  go acc _ _ | acc `seq` False = error "search: making accumulator strict"
  go acc [] _ = acc
  go acc (q:qs) defined
    | q `Set.member` defined = go acc qs defined
    | otherwise = case find (isDeclarant trav) pols of

          Nothing  -> error ("Unknown entity: " ++ str) -- Likely a bug in the search tool

          Just m ->
            let
              modId                 = m ^. moduleDef . moduleId
              allModuleRequirements = m ^.. entities

            in go (Set.insert modId acc)        -- add this module to the result set
                  (allModuleRequirements ++ qs) -- add discovered requirements to queue
                  (Set.insert q defined)        -- add current entity to defined set
    where
    -- trav is a traversal that only traverses the entity in question
    (str, trav) = case q of
        EType      t -> (show t, _Type . tinplate . only t)
        ERole      r -> (show r, _Role . _1 . only r)
        EAttribute a -> (show a, _Attribute . only a)

-- Traverse the required requirement constructors in an avrule
requiredAvRuleReferences :: Fold S.AvRule S.Require
requiredAvRuleReferences = _AvRuleRequire . folded

-- Traverse the required and optional requirement constructors in an avrule
allAvRuleReferences :: Fold S.AvRule S.Require
allAvRuleReferences f (S.TeRbac teRbac)    = coerce ((_Optional . _1 . avRules . folded . allAvRuleReferences) f teRbac)
allAvRuleReferences f (S.AvRuleRequire xs) = coerce (traverse f xs)

-- collect the entities defined by a requirement
wrappedRequirements :: Fold S.Require Entity
wrappedRequirements f (S.RequireType      t) = coerce ((folded . to EType     ) f t)
wrappedRequirements f (S.RequireRole      r) = coerce ((folded . to ERole     ) f r)
wrappedRequirements f (S.RequireAttribute a) = coerce ((folded . to EAttribute) f a)
wrappedRequirements f s                      = ignored f s

isDeclarant :: ATraversal' S.TeRbac () -> S.Module -> Bool
isDeclarant con = has (moduleAvRules . allTeRbacs)
  where
  allTeRbacs :: Fold S.AvRuleBlock ()
  allTeRbacs = avRules . folded . _TeRbac . aux

  aux :: Fold S.TeRbac ()
  aux f (S.Optional x _) = coerce (allTeRbacs f x)
  aux f x                = cloneTraversal con f x

loadModule :: FilePath -> IO S.Module
loadModule fn = do
  hPutStrLn stderr ("Loading module " ++ fn)
  txt <- readFile fn
  case parseModule fn txt of
    Left e   -> fail e
    Right xs -> return xs

loadPolicy :: FilePath -> IO S.Policy
loadPolicy fn = do
  hPutStrLn stderr ("Loading policy " ++ fn)
  txt <- readFile fn
  case parsePolicy fn txt of
    Left e   -> fail e
    Right xs -> return xs

------------------------------------------------------------------------
-- Lenses and Prisms for Policies and Modules
------------------------------------------------------------------------

-- Can't automate these because they don't have field names

avRules :: Lens' S.AvRuleBlock [S.AvRule]
avRules = lens (\ (S.AvRuleBlock xs _) -> xs) (\(S.AvRuleBlock _ us) xs -> S.AvRuleBlock xs us)

moduleId :: Lens' S.ModuleDef S.ModuleId
moduleId = lens (\(S.ModuleDef i _) -> i) $ \(S.ModuleDef _ v) b -> S.ModuleDef b v
