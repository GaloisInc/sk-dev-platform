{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  $Header$
Description :  Information flow graphs
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Information flow graphs consisting of connected nested security domains.
-}
module Lobster.Domain
  ( prettyPrintDirection
  , PortTypeValue(..)
  , PortType(..) -- fixme: the ellipses break the intended abstraction barrier, but needed for LViz module
  , anyPortType
  , singletonPortType
  , lookupPortType
  , unifyPortType
  , directionPortType
  , prettyPrintPortType
  , DomainId
  , domainDomainPort
  , portDomainPort
  , externalDomainPort
  , internalDomainPort
  , isExternalDomainPort
  , equalInternalDomainPort
  , prettyPrintDomainPort
  , prettyPrintDomainPortId
  , empty
  , DomainPort(..) -- fixme: the ellipses break the intended abstraction barrier, but needed for LViz module
  , Domain(..) -- fixme: the ellipses break the intended abstraction barrier, but needed for LViz module
--   , name
--   , value
--   , ports
  , getPortType
  , addPort
  , getSubDomain
  , addSubDomain
  , foldSubDomain
  , foldMSubDomain
  , addConnections
  , addPortConnections
  , foldMConnections
  , appConnections
  , typeCheck
  , flatten
  , prettyPrint
  , PortRE(..)
  , Assertion(..)
  , Pred(..)
  , addAssertion
  , checkAssertions_
  ) where

import Control.Monad(liftM,foldM)
import Control.DeepSeq

--import qualified Data.Char as Char
import qualified Data.List as List
--import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import Data.List
import Data.Maybe
-- import Debug.Trace

import Lobster.Monad
--import qualified Lobster.Abs as Abs
import Lobster.Abs(
  Connection(..),
  Direction(..),
  FlowId,
  PortId,
  Position,
  ClassId
  )
import qualified Lobster.Syntax as Syntax
import Lobster.Symbion as Symbion

--------------------------------------------------------------------------------
-- Connections.
--------------------------------------------------------------------------------

-- data Connection =
--     NeutralConnection
--   | LeftToRightConnection
--   | RightToLeftConnection
--   | BidirectionalConnection
--   deriving (Eq, Read, Show, Ord)

prettyPrintConnection :: Connection -> String
prettyPrintConnection conn =
    case conn of
      NeutralConnection -> "--"
      LeftToRightConnection -> "-->"
      RightToLeftConnection -> "<--"
      BidirectionalConnection -> "<-->"

reverseConnection :: Connection -> Connection
reverseConnection LeftToRightConnection = RightToLeftConnection
reverseConnection RightToLeftConnection = LeftToRightConnection
reverseConnection c = c

-- The kind of a connection dominates the kind of a neutral connection.
-- When two connections have the same kind, they can be unified,
-- otherwise an error has occurred.
unifyConnection :: Connection -> Connection -> P Connection
unifyConnection c1 c2 =
    case (c1,c2) of
      (NeutralConnection,_) -> return c2
      (_,NeutralConnection) -> return c1
      _ -> if c1 == c2 then return c1
           else throwError ("incompatible connections: " ++ show c1 ++
                            " and " ++ show c2)

unifyMaybeConnection :: Maybe Connection -> Connection -> P Connection
unifyMaybeConnection mc1 c2 =
    case mc1 of
      Nothing -> return c2
      Just c1 -> unifyConnection c1 c2

--------------------------------------------------------------------------------
-- (Main) direction of data flow at a port.
--------------------------------------------------------------------------------

-- data Direction =
--     InputDirection
--   | OutputDirection
--   | BidirectionalDirection
--   deriving (Eq, Read, Show, Ord)

unifyDirection :: Direction -> Direction -> Maybe Direction
unifyDirection d1 d2 =
    if d1 == d2
      then Just d1
      else Nothing

originDirection :: Connection -> Maybe Direction
originDirection conn =
    case conn of
      BidirectionalConnection -> Just BidirectionalDirection
      LeftToRightConnection -> Just OutputDirection
      RightToLeftConnection -> Just InputDirection
      NeutralConnection -> Nothing

invertDirection :: Direction -> Direction
invertDirection d =
    case d of
      InputDirection -> OutputDirection
      OutputDirection -> InputDirection
      BidirectionalDirection -> BidirectionalDirection

-- | Check to see if two directions can be connected to eachother.
connectableDirection :: Direction -> Direction -> Bool
connectableDirection d1 d2 = d2 == invertDirection d1

prettyPrintDirection :: Direction -> String
prettyPrintDirection direction =
    case direction of
      InputDirection -> "input"
      OutputDirection -> "output"
      BidirectionalDirection -> "input/output"

--------------------------------------------------------------------------------
-- Information flow property values.
--------------------------------------------------------------------------------

data PortTypeValue a =
    Inconsistent
  | Direction Direction
  | Value a
  deriving (Eq, Show, Ord)

consistent :: PortTypeValue a -> Bool
consistent Inconsistent = False
consistent _            = True

instance NFData a => NFData (PortTypeValue a) where
  rnf x = case x of
    Inconsistent -> ()
    Direction a -> rnf a
    Value a -> rnf a

unifyPortTypeValue ::
    (a -> a -> Maybe a) -> PortTypeValue a -> PortTypeValue a -> PortTypeValue a
unifyPortTypeValue uv v1 v2 =
    case (v1,v2) of
      (Direction d1, Direction d2) ->
          case unifyDirection d1 d2 of
            Just d -> Direction d
            Nothing -> Inconsistent
      (Value t1, Value t2) ->
          case uv t1 t2 of
            Just t -> Value t
            Nothing -> Inconsistent
      _ -> Inconsistent

connectablePortTypeValue ::
    (a -> a -> Bool) -> PortTypeValue a -> PortTypeValue a -> Bool
connectablePortTypeValue connectable v1 v2 =
    case (v1,v2) of
      (Direction d1, Direction d2) -> connectableDirection d1 d2
      (Value t1, Value t2) -> connectable t1 t2
      _ -> False

prettyPrintPortTypeValue :: (a -> String) -> PortTypeValue a -> String
prettyPrintPortTypeValue v2s v =
    (case v of
       Inconsistent -> "*"
       Direction d -> prettyPrintDirection d
       Value t -> v2s t)

--------------------------------------------------------------------------------
-- A collection of information flow properties.
--------------------------------------------------------------------------------

data PortType a =
    PortType (Map.Map FlowId (PortTypeValue a))
  deriving (Eq, Show, Ord)

instance NFData a => NFData (PortType a) where
  rnf (PortType a) = rnf a

anyPortType :: PortType a
anyPortType = PortType Map.empty

singletonPortType :: FlowId -> PortTypeValue a -> PortType a
singletonPortType f v = PortType (Map.singleton f v)

lookupPortType :: PortType a -> FlowId -> Maybe (PortTypeValue a)
lookupPortType (PortType m) f = Map.lookup f m

addPortType ::
    (a -> a -> Maybe a) -> FlowId -> PortTypeValue a ->
    PortType a -> PortType a
addPortType uv f v (PortType m) =
    let v' = case Map.lookup f m of
               Just v'' -> unifyPortTypeValue uv v v''
               Nothing -> v in
    PortType (Map.insert f v' m)

unifyPortType ::(a -> a -> Maybe a) -> PortType a -> PortType a -> PortType a
unifyPortType uv (PortType m1) pt2 =
    foldr add pt2 (Map.toList m1)
    where
      add (f,v) pt = addPortType uv f v pt

connectableFlowPortType ::
    (a -> a -> Bool) -> FlowId -> PortTypeValue a -> PortType a -> Bool
connectableFlowPortType connectable f v (PortType m) =
    -- The bug is here! (or at least a contributing factor).  The
    -- incoming port type valaue is 'Inconsistent', which would fail
    -- to be connectable to any PortType value, however, the incomming
    -- port type *has no port type values* so no comparisons are done,
    -- and this returns True.
    consistent v && checkMap
     where checkMap = case Map.lookup f m of
                        Just v' -> connectablePortTypeValue connectable v v'
                        Nothing -> True

-- | connectablePortType determines if two 'PortType's are compatible
-- (can pass data from one to the other).  If the connectable relation
-- is symmetric, then connectablePortType will also be symmetric.
-- 
-- TODO ERC: This is a prime candidate for random testing
connectablePortType :: (a -> a -> Bool) -> PortType a -> PortType a -> Bool
connectablePortType connectable (PortType m1) pt2@(PortType m2) =
    all consistent (Map.elems m2) && all check (Map.toList m1)
    where
      check (f,v) = connectableFlowPortType connectable f v pt2

directionPortType :: PortType a -> Maybe Direction
directionPortType pt = 
    case lookupPortType pt Syntax.directionFlow of
      Just (Direction d) -> Just d
      _                  -> Nothing

prettyPrintPortType :: (a -> String) -> PortType a -> String
prettyPrintPortType v2s (PortType m) =
    "{" ++ List.intercalate ", " (map pp (Map.toList m)) ++ "}"
    where
      pp (f,v) = Syntax.idString f ++ " = " ++ prettyPrintPortTypeValue v2s v

--------------------------------------------------------------------------------
-- Port directions.
--------------------------------------------------------------------------------


originPortType :: Connection -> PortType a
originPortType c =
    case originDirection c of
      Just d -> mkDirPortType d
      Nothing -> anyPortType
  where mkDirPortType :: Direction -> PortType a
        mkDirPortType d = singletonPortType Syntax.directionFlow (Direction d)

--------------------------------------------------------------------------------
-- Subdomain IDs.
--------------------------------------------------------------------------------

newtype DomainId = DomainId Int
  deriving (Eq, Show, Ord, Num)

instance NFData DomainId where
  rnf (DomainId i) = rnf i

--------------------------------------------------------------------------------
-- Subdomain ports.
--------------------------------------------------------------------------------

data DomainPort =
    DomainPort
      {domain :: Maybe DomainId,
       port :: PortId}
  deriving (Eq, Show, Ord)

instance NFData DomainPort where
  rnf (DomainPort a b) = rnf a `seq` rnf b

instance NFData PortId where
  rnf = rnf . show

instance NFData FlowId where
  rnf = rnf . show

instance NFData Connection where
  rnf = rnf . show

instance NFData Position where
  rnf = rnf . show

instance NFData ClassId where
  rnf = rnf . show

instance NFData Direction where
  rnf = rnf . show

domainDomainPort :: DomainPort -> Maybe DomainId
domainDomainPort = domain

portDomainPort :: DomainPort -> PortId
portDomainPort = port

externalDomainPort :: PortId -> DomainPort
externalDomainPort pid = DomainPort {domain = Nothing, port = pid}

internalDomainPort :: DomainId -> PortId -> DomainPort
internalDomainPort oid pid = DomainPort {domain = Just oid, port = pid}

isExternalDomainPort :: DomainPort -> Bool
isExternalDomainPort (DomainPort {domain = o}) =
    case o of
      Nothing -> True
      Just _ -> False

equalInternalDomainPort :: DomainId -> DomainPort -> Bool
equalInternalDomainPort o (DomainPort {domain = o'}) =
    case o' of
      Nothing -> False
      Just o'' -> o == o''

prettyPrintDomainPort :: Domain a b -> DomainPort -> String
prettyPrintDomainPort obj dp =
  (maybe "" (prettyPrintDomainPortId obj) (domain dp)) ++ "." ++ Syntax.idString (port dp)

prettyPrintDomainPortId :: Domain a b -> DomainId -> String
prettyPrintDomainPortId obj i = case lookupSubDomain obj i of
  Nothing -> error $ "unknown sub-domain id:" ++ show i
  Just si -> name si

--------------------------------------------------------------------------------
-- Domains.
--------------------------------------------------------------------------------

data Domain a b = Domain
  { name :: String
  , value :: a
  , ports :: Map.Map PortId (PortType b)
  , subDomains :: Map.Map DomainId (Domain a b)
  , connections :: Map.Map (DomainPort,DomainPort) Connection
  , assertions :: [Assertion]
  }

instance (NFData a, NFData b) => NFData (Domain a b) where
  rnf (Domain a b c d e f) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f

--------------------------------------------------------------------------------
-- Assertions.
--------------------------------------------------------------------------------

data Assertion = Assertion String Pred
  deriving Show

instance NFData Assertion where
  rnf (Assertion a b) = rnf a `seq` rnf b

data Pred
  = NoPathPred PortRE PortRE
  | IsPathPred PortRE PortRE
  | ViaPred PortRE PortRE PortRE
  | AndPred Pred Pred
  | OrPred Pred Pred
  deriving Show

data SymbPTy = InP | OutP deriving (Show,Eq)

data SymbP = SymbP (Maybe DomainId) PortId SymbPTy deriving (Show,Eq)

instance NFData Pred where
  rnf x = case x of
    NoPathPred a b -> rnf a `seq` rnf b
    IsPathPred a b -> rnf a `seq` rnf b
    ViaPred a b c -> rnf a `seq` rnf c `seq` rnf b
    AndPred a b -> rnf a `seq` rnf b
    OrPred a b -> rnf a `seq` rnf b

data PortRE
  = ThisAnyPortRE
  | AnyPortRE DomainId
  | PortRE DomainPort
  deriving Show

instance NFData PortRE where
  rnf x = case x of
    ThisAnyPortRE -> ()
    AnyPortRE a -> rnf a
    PortRE a -> rnf a

empty :: String -> a -> Domain a b
empty n v = Domain
  { name = n,
    value = v,
    ports = Map.empty,
    subDomains = Map.empty,
    connections = Map.empty,
    assertions = []
  }

prefixNameDomain :: Domain a b -> String -> Domain a b
prefixNameDomain obj prefix =
    let n = name obj in
    if prefix == "" then obj
    else if n == "" then obj {name = prefix}
    else obj {name = prefix ++ "_" ++ n}

isPrimitiveDomain :: Domain a b -> Bool
isPrimitiveDomain obj = Map.null (subDomains obj)

lookupPortDomain :: Domain a b -> PortId -> Maybe (PortType b)
lookupPortDomain obj pid = Map.lookup pid (ports obj)

getPortDomain :: Domain a b -> PortId -> P (PortType b)
getPortDomain obj pid =
    case lookupPortDomain obj pid of
      Just p -> return p
      Nothing -> throwError $ "no such port: " ++ show pid

addPort :: Domain a b -> PortId -> PortType b -> P (Domain a b)
addPort obj pid pty =
    case lookupPortDomain obj pid of
      Nothing -> return (obj {ports = Map.insert pid pty (ports obj)})
      Just _ -> throwError $ "duplicate port declaration " ++ show pid

updatePortDomain :: Domain a b -> PortId -> PortType b -> P (Domain a b)
updatePortDomain obj pid pty =
    case lookupPortDomain obj pid of
      Nothing -> throwError $ "duplicate port declaration " ++ show pid
      Just _ -> return (obj {ports = Map.insert pid pty (ports obj)})

lookupSubDomain :: Domain a b -> DomainId -> Maybe (Domain a b)
lookupSubDomain obj oid = Map.lookup oid (subDomains obj)

getSubDomain :: Domain a b -> DomainId -> P (Domain a b)
getSubDomain obj oid =
    case lookupSubDomain obj oid of
      Nothing -> throwError $ "no such sub-domain '" ++ show oid ++
                              "' in domain '" ++ name obj++"'"
      Just o -> return o

nextSubDomain :: Domain a b -> DomainId
nextSubDomain obj =
    DomainId
      (let subObjs = subDomains obj in
       if Map.null subObjs
         then 0
         else let (DomainId i, _) = Map.findMax subObjs in
              i + 1)

addSubDomain :: Domain a b -> Domain a b -> (Domain a b, DomainId)
addSubDomain obj subObj =
    let oid = nextSubDomain obj in
    let obj' = obj {subDomains = Map.insert oid subObj (subDomains obj)} in
    (obj',oid)

deleteSubDomain :: Domain a b -> DomainId -> P (Domain a b)
deleteSubDomain obj oid =
    case lookupSubDomain obj oid of
      Nothing -> throwError $ "no such sub-domain" ++ show oid
      Just _ -> return (obj {subDomains = Map.delete oid (subDomains obj)})

updateSubDomain :: Domain a b -> DomainId -> Domain a b -> P (Domain a b)
updateSubDomain obj i si =
    case lookupSubDomain obj i of
      Nothing -> throwError $ "no such sub-domain" ++ show i
      Just _ -> return (obj {subDomains = Map.insert i si (subDomains obj)})

mapMSubDomain :: (Domain a b -> P (Domain a b)) -> Domain a b -> P (Domain a b)
mapMSubDomain f obj =
    let Domain {subDomains = s} = obj in
    do s' <- Traversable.mapM f s
       return (obj {subDomains = s'})

foldSubDomain :: (DomainId -> Domain a b -> s -> s) -> s -> Domain a b -> s
foldSubDomain f x obj = Map.foldWithKey f x (subDomains obj)

foldMSubDomain :: (DomainId -> Domain a b -> s -> P s) -> s -> Domain a b -> P s
foldMSubDomain f =
    \x obj -> foldM f' x (Map.toList (subDomains obj))
    where
      f' z (i,o) = f i o z

{-
foldPortDomain :: (PortId -> PortType -> a -> a) -> a -> Domain -> a
foldPortDomain f x obj = Map.foldWithKey f x (ports obj)
-}

foldConnectionsDomain ::
    (DomainPort -> Connection -> DomainPort -> s -> s) ->
    s -> Domain a b -> s
foldConnectionsDomain f =
    \x obj -> Map.foldWithKey f' x (connections obj)
    where
      f' (p,q) c z = f p c q z

foldMConnections ::
    (DomainPort -> Connection -> DomainPort -> s -> P s) ->
    s -> Domain a b -> P s
foldMConnections f =
    \x obj -> foldM f' x (Map.toList (connections obj))
    where
      f' z ((p,q),c) = f p c q z

appConnections ::
    (DomainPort -> Connection -> DomainPort -> P ()) -> Domain a b -> P ()
appConnections f =
    foldMConnections f' ()
    where
      f' p c q () = f p c q

lookupConnectionDomain ::
    Domain a b -> DomainPort -> DomainPort -> Maybe Connection
lookupConnectionDomain obj p1 p2 =
    case compare p1 p2 of
      LT -> Map.lookup (p1,p2) (connections obj)
      EQ -> Nothing
      GT -> liftM reverseConnection (Map.lookup (p2,p1) (connections obj))

addConnectionDomain ::
    Domain a b -> DomainPort -> Connection -> DomainPort -> P (Domain a b)
addConnectionDomain obj p1 c p2 =
    do c' <- unifyMaybeConnection (lookupConnectionDomain obj p1 p2) c
       case compare p1 p2 of
         LT -> let m = connections obj
                   m' = Map.insert (p1,p2) c' m in
               return (obj {connections = m'})
         EQ -> throwError $ "can't connect a port to itself " ++ show p1
         GT -> let m = connections obj
                   m' = Map.insert (p2,p1) (reverseConnection c') m in
               return (obj {connections = m'})

addConnections ::
    Domain a b -> DomainPort -> Connection -> [DomainPort] ->
    P (Domain a b)
addConnections =
    \obj p1 conn ps2 -> foldM (add p1 conn) obj ps2
    where
      add p1 conn obj p2 = addConnectionDomain obj p1 conn p2

addDirectedConnectionsDomain ::
    Domain a b -> DomainPort -> Connection ->
    [(Connection,DomainPort)] -> P (Domain a b)
addDirectedConnectionsDomain =
    \obj p1 conn ps2 -> foldM (add p1 conn) obj ps2
    where
      add p1 conn obj (c2,p2) =
          do conn' <- unifyConnection conn c2
             addConnectionDomain obj p1 conn' p2

addPortConnections ::
    Domain a b -> [DomainPort] -> Connection -> [DomainPort] ->
    P (Domain a b)
addPortConnections =
    \obj ps1 conn ps2 -> foldM (add conn ps2) obj ps1
    where
      add conn ps2 obj p1 = addConnections obj p1 conn ps2

addPortDirectedConnectionsDomain ::
    Domain a b -> [(Connection,DomainPort)] -> Connection ->
    [(Connection,DomainPort)] -> P (Domain a b)
addPortDirectedConnectionsDomain =
    \obj ps1 conn ps2 -> foldM (add conn ps2) obj ps1
    where
      add conn ps2 obj (c1,p1) =
          let c1' = reverseConnection c1 in
          do conn' <- unifyConnection conn c1'
             addDirectedConnectionsDomain obj p1 conn' ps2

getPortType :: Domain a b -> DomainPort -> P (PortType b)
getPortType obj (DomainPort {domain = o, port = p}) =
    case o of
      Just i ->
          do si <- getSubDomain obj i
             getPortDomain si p
      Nothing -> getPortDomain obj p

updatePortTypeDomain :: Domain a b -> DomainPort -> PortType b -> P (Domain a b)
updatePortTypeDomain obj (DomainPort {domain = o, port = p}) t =
    case o of
      Just i ->
          do si <- getSubDomain obj i
             si' <- updatePortDomain si p t
             updateSubDomain obj i si'
      Nothing ->
          updatePortDomain obj p t

liftConnectionPortTypeDomain ::
    (b -> b -> Maybe b) -> (b -> b -> Bool) -> (b -> String) ->
    DomainPort -> Connection -> DomainPort ->
    Domain a b -> P (Domain a b)
liftConnectionPortTypeDomain unify connectable pp p1 conn p2 obj =
    case (isExternalDomainPort p1, isExternalDomainPort p2) of
      (False,False) ->
          do t1 <- getPortType obj p1
             t2 <- getPortType obj p2
             let t = unifyPortType unify t1 (originPortType conn)
             if connectablePortType connectable t t2
               then return obj
               else throwError
                      ("incompatible connection in domain " ++
                       show (name obj) ++ ":\n" ++
                       "in connection " ++ prettyPrintDomainPort obj p1 ++
                       " " ++ prettyPrintConnection conn ++ " " ++
                       prettyPrintDomainPort obj p2 ++ "\nwith port types " ++
                       prettyPrintPortType pp t1 ++ "\nand " ++
                       prettyPrintPortType pp t2)
      (True,True) -> throwError
                       ("internal connection in domain " ++
                        show (name obj) ++ ": " ++
                        prettyPrintDomainPort obj p1 ++
                        " " ++ prettyPrintConnection conn ++ " " ++
                        prettyPrintDomainPort obj p2)
      (True,False) ->
          let conn' = reverseConnection conn in
          liftConnectionPortTypeDomain unify connectable pp p2 conn' p1 obj
      (False,True) ->
          do t1 <- getPortType obj p1
             t2 <- getPortType obj p2
             let t = unifyPortType unify t1 t2
             let t' = unifyPortType unify t (originPortType conn)
             updatePortTypeDomain obj p2 t'

liftPortTypesDomain ::
    (b -> b -> Maybe b) -> (b -> b -> Bool) -> (b -> String) ->
    Domain a b -> P (Domain a b)
liftPortTypesDomain unify connectable pp obj =
    do obj' <- mapMSubDomain (liftPortTypesDomain unify connectable pp) obj
       foldMConnections
         (liftConnectionPortTypeDomain unify connectable pp) obj' obj'

typeCheck ::
    (b -> b -> Maybe b) -> (b -> b -> Bool) -> (b -> String) ->
    Domain a b -> P ()
typeCheck unify connectable pp obj =
    do _ <- liftPortTypesDomain unify connectable pp obj
       return ()

explodeSubDomain :: Domain a b -> DomainId -> Domain a b -> P (Domain a b)
explodeSubDomain =
    \obj subId subObj ->
    if isPrimitiveDomain subObj
      then return obj
      else
        let (intObj,obj') =
              foldSubDomain (addSubObj (name subObj)) (Map.empty,obj) subObj in
        do obj'' <- deleteSubDomain obj' subId
           (intPort,obj''') <-
               foldMConnections (addSubConn intObj)
                 (Map.empty, obj'' {connections = Map.empty}) subObj
           foldMConnections (expand subId intPort) obj''' obj

    where
      addSubObj subPrefix subId subObj (intObj,obj) =
          let subObj' = prefixNameDomain subObj subPrefix in
          let (obj',subId') = addSubDomain obj subObj' in
          let intObj' = Map.insert subId subId' intObj in
          (intObj',obj')

      getIntObj intObj o =
          case Map.lookup o intObj of
            Nothing -> throwError "getIntObj"
            Just o' -> return o'

      getIntPort intPort p =
          case Map.lookup p intPort of
            Just ps -> ps
            Nothing -> []

      addSubConn intObj p1 conn p2 (intPort,obj) =
          case (domain p1, domain p2) of
            (Nothing,Nothing) -> throwError "internal connection"
            (Just _, Nothing) ->
                let conn' = reverseConnection conn in
                addSubConn intObj p2 conn' p1 (intPort,obj)
            (Nothing, Just o2) ->
                do o2' <- getIntObj intObj o2
                   let p2' = p2 {domain = Just o2'}
                       p = port p1
                       ps = getIntPort intPort p
                       intPort' = Map.insert p ((conn,p2') : ps) intPort
                   return (intPort',obj)
            (Just o1, Just o2) ->
                do o1' <- getIntObj intObj o1
                   o2' <- getIntObj intObj o2
                   let p1' = p1 {domain = Just o1'}
                       p2' = p2 {domain = Just o2'}
                   obj' <- addConnectionDomain obj p1' conn p2'
                   return (intPort,obj')

      expandPort subId intPort p =
          if equalInternalDomainPort subId p
            then getIntPort intPort (port p)
            else [(NeutralConnection,p)]

      expand subId intPort p1 conn p2 obj =
          let ps1 = expandPort subId intPort p1
              ps2 = expandPort subId intPort p2 in
          addPortDirectedConnectionsDomain obj ps1 conn ps2

flatten :: Domain a b -> P (Domain a b)
flatten =
    \obj ->
    do obj' <- mapMSubDomain flatten obj
       foldMSubDomain explode obj' obj'

    where
      explode oid o obj = explodeSubDomain obj oid o

prettyPrint :: (a -> String) -> (b -> String) -> Domain a b -> String
prettyPrint ppV ppPTV =
    pp ""
    where
      pp ind obj =
          let ind' = ind ++ "  " in
          ind ++ show (name obj) ++ " " ++
          ppV (value obj) ++ " {\n" ++
          foldSubDomain (\_ o z -> pp ind' o ++ z) "" obj ++
          concat (map (ppPort ind') (Map.toList (ports obj))) ++
          foldConnectionsDomain
            (\p c q z -> ppConnection obj ind' p c q ++ z) "" obj ++
          ind ++ "}\n"

      ppPort ind pt = ind ++ "port " ++ ppPT pt ++ ";\n"

      ppConnection obj ind p c q =
          ind ++ prettyPrintDomainPort obj p ++ " " ++
          prettyPrintConnection c ++ " " ++
          prettyPrintDomainPort obj q ++ ";\n"

      ppPT (p,t) = Syntax.idString p ++ " : " ++ prettyPrintPortType ppPTV t

--------------------------------------------------------------------------------
-- Symbion assertions.
--------------------------------------------------------------------------------

prettyPrintSymbP :: Domain a b -> SymbP -> String
prettyPrintSymbP dom (SymbP md p t) =
  prettyPrintDomainPort dom (DomainPort md p) ++ "(" ++ prettyPrintSymbPTy t ++ ")"

prettyPrintSymbPTy :: SymbPTy -> String
prettyPrintSymbPTy x = case x of
  InP -> "in"
  OutP -> "out"

domainToGraph :: Domain a b -> LGraph SymbP
domainToGraph d = Symbion.mkLGraph (prettyPrintSymbP d) nodes edges
  where
  nodes = concatMap domainPortToSymbPs $ allPorts d
  edges =
    explicitConnections ++
    concatMap implicitConnections (Map.toList $ subDomains d)
  explicitConnections = concatMap mkLeftToRight $ Map.toList $ connections d

domainPortToSymbPs :: DomainPort -> [SymbP]
domainPortToSymbPs p = [domainPToInP p, domainPToOutP p]

implicitConnections :: (DomainId,Domain a b) -> [(SymbP,SymbP)]
implicitConnections (domId,dom) = allPaths \\ neverPaths
  where
  allPaths = [ (domainPToInP p, domainPToOutP q) | p <- dps, q <- dps ]
  dps = [ internalDomainPort domId x | x <- Map.keys $ ports dom ]
  neverPaths = concat [ implicitNeverPaths (expandImplicitPortRE domId dom) p
                        | Assertion _ p <- assertions dom ]

expandImplicitPortRE :: DomainId -> Domain a b -> PortRE -> [DomainPort]
expandImplicitPortRE domId dom x = case x of
  ThisAnyPortRE -> [ p{ domain = Just domId } | p <- externalPorts dom ]
  PortRE p | isNothing (domain p) -> [p{ domain = Just domId }]
  _ -> []

implicitNeverPaths :: (PortRE -> [DomainPort]) -> Pred -> [(SymbP,SymbP)]
implicitNeverPaths f x = nub $ case x of
  NoPathPred a b -> [ (domainPToInP p, domainPToOutP q) | p <- f a, q <- f b ]
  AndPred a b -> implicitNeverPaths f a `union` implicitNeverPaths f b
  OrPred a b -> implicitNeverPaths f a `intersect` implicitNeverPaths f b
  ViaPred _ _ _ -> []
  IsPathPred _ _ -> []

mkLeftToRight :: ((DomainPort,DomainPort), Connection) -> [(SymbP,SymbP)]
mkLeftToRight ((a,b),c) = case c of
  BidirectionalConnection ->
    [(domainPToOutP a, domainPToInP b)
    ,(domainPToOutP b, domainPToInP a)]
  LeftToRightConnection -> [(domainPToOutP a, domainPToInP b)]
  RightToLeftConnection -> mkLeftToRight ((b,a),LeftToRightConnection)
  NeutralConnection -> mkLeftToRight ((a,b),BidirectionalConnection)

domainPToSymbP :: SymbPTy -> DomainPort -> SymbP
domainPToSymbP c (DomainPort a b) = SymbP a b c

domainPToInP :: DomainPort -> SymbP
domainPToInP = domainPToSymbP InP

domainPToOutP :: DomainPort -> SymbP
domainPToOutP = domainPToSymbP OutP

addAssertion :: Domain a b -> Assertion -> Domain a b
addAssertion obj a = obj { assertions = a : assertions obj }

checkAssertions_ :: Domain a b -> [Either String String]
checkAssertions_ = checkAssertions domainToGraph expandPortRESymbP

checkAssertions :: Eq c =>
  (Domain a b -> LGraph c) -> (Domain a b -> PortRE -> [c]) -> Domain a b ->
  [Either String String]
checkAssertions f g d = nub $ es1 ++ es2
  where
  es1 = map (checkAssertion (g d) gr) $ assertions d
  es2 = concat $ map (checkAssertions f g) $ Map.elems $ subDomains d
  gr = f d

checkAssertion :: Eq a => (PortRE -> [a]) -> LGraph a -> Assertion -> Either String String
checkAssertion f g a@(Assertion s _) = case evalGrPred grPred of
  Ok -> Right $ "SUCCESS: assertion passed: " ++ s
  Err e -> Left $ s ++ ":" ++ ppErr e
  where
  grPred = compileAssertion f g a

compileAssertion :: Eq a => (PortRE -> [a]) -> LGraph a -> Assertion -> GrPred a
compileAssertion f g (Assertion orig p) = case p of
  NoPathPred a b -> foldl1 AndP [ NoPathP x y g | x <- f a, y <- f b ]
  IsPathPred a b -> foldl1 OrP [ IsPathP x y g | x <- f a, y <- f b ]
  ViaPred a b c ->
    compileAssertion f (deleteNodes (f c) g) (Assertion orig (NoPathPred a b))
  AndPred pa pb -> AndP
    (compileAssertion f g (Assertion orig pa))
    (compileAssertion f g (Assertion orig pb))
  OrPred pa pb -> OrP
    (compileAssertion f g (Assertion orig pa))
    (compileAssertion f g (Assertion orig pb))

expandPortRESymbP :: Domain a b -> PortRE -> [SymbP]
expandPortRESymbP d x = concatMap domainPortToSymbPs $ expandPortRE d x

expandPortRE :: Domain a b -> PortRE -> [DomainPort]
expandPortRE d x = case x of
  ThisAnyPortRE -> externalPorts d
  AnyPortRE i -> lookupInternalPorts d i
  PortRE p -> [p]

allPorts :: Domain a b -> [DomainPort]
allPorts d = nub (externalPorts d ++ internalPorts d)

externalPorts :: Domain a b -> [DomainPort]
externalPorts d = nub (map externalDomainPort $ Map.keys $ ports d)

internalPorts :: Domain a b -> [DomainPort]
internalPorts d = nub (concatMap (lookupInternalPorts d) $ Map.keys $ subDomains d)

lookupInternalPorts :: Domain a b -> DomainId -> [DomainPort]
lookupInternalPorts d i = case lookupSubDomain d i of
  Just d1 -> map (internalDomainPort i) $ Map.keys $ ports d1
  Nothing -> error "panic:unknown domain id"
