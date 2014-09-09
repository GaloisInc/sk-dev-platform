{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Generate Lobster policy from symbolic IO execution of a program.
module SCD.Lobster.Symbolic where

import Data.Symbolic.IO(M, Trace, Step(..), runGenerate)
import SCD.Lobster.Syntax.Abs
import Control.Monad.State.Lazy(StateT, MonadState, get, put, runStateT)
import Control.Monad.Reader(ReaderT, MonadReader, ask, runReaderT)
import Control.Monad.Identity(Identity, runIdentity)

newtype S a = S (StateT Integer (ReaderT Identifier Identity) a)
  deriving (Monad, MonadState Integer, MonadReader Identifier, Functor)

runS :: Identifier -> S a -> a
runS dom (S m) =  fst (runIdentity (runReaderT (runStateT m 0) dom))

generate :: M a -> IO Policy
generate = fmap trace2Policy . runGenerate

trace2Policy :: Trace -> Policy
trace2Policy = Policy . runS mydom . distill
  where mydom = identifier "d"

distill :: Trace -> S [Statement]
distill = fmap (simplify . concat) . mapM distillStep

simplify :: [Statement] -> [Statement]
simplify ss = toList (fromList (map (substitute substmap) ss))
  where
    dg = map (map dname . snd) (groupSort dpart doms)
    substlist (n:l) = [(n',n) | n' <- l]
    substmap = fromList (map substlist dg)

    doms = [(n,c,params) | DomainDeclaration n c params <- ss]
    dpart (n,c,params) = (c,params)
    dname (n,c,params) = n

distillStep :: Step -> S [Statement]
distillStep (ReadFile f)    = connectMe (newFileDomain f) [readPort]
distillStep (WriteFile f _) = connectMe (newFileDomain f) [writePort]

newFileDomain :: FilePath -> S (Identifier, [Statement])
newFileDomain f = newDomain fileClass [StringExpression f]

newDomain :: ClassId -> [Expression] -> S (Identifier, [Statement])
newDomain c params = do
  n <- newId "d"
  return (n,[DomainDeclaration n c params])

connectMe :: S (Identifier, [Statement]) -> [PortId] -> S [Statement]
connectMe nd ports = do
  (n, s) <- nd 
  di <- myDomain
  return (s++[ PortConnection [di .+. activePort] NeutralConnection [n .+. p] 
             | p <- ports ])

(.+.) :: (HasIdentifier a, HasIdentifier b) => a -> b -> Expression
a .+. b = QualNameExpression (Qual (Ident (ident a)) (Ident (ident b)))

newId :: String -> S Identifier
newId p = do
  n <- get
  put (n+1)
  return (identifier (p++(show n)))

myDomain :: S Identifier
myDomain = ask

readPort :: PortId
readPort = portId "read"

writePort :: PortId
writePort = portId "write"

activePort :: PortId
activePort = portId "active"

portId :: String -> PortId
portId = PortId . LIdent

fileClass :: ClassId
fileClass = classId "File"

classId :: String -> ClassId
classId = ClassId . UIdent

identifier :: String -> Identifier
identifier = Identifier . LIdent

class HasIdentifier a where
  ident :: a -> Identifier

instance HasIdentifier PortId where ident (PortId i) = Identifier i
instance HasIdentifier Identifier where ident = id

class Substitute f t e where
  substitute :: Map f t -> e -> e


instance Substitute f t e -> Substitute f t [e] where
  substitute = map . substitute

instance Substitute Identifier Identifier Statement where
  substitute m (ClassDeclaration c l s) = ClassDeclaration c (substitute m l) s
  substitute m s@(TypeDeclaration _) = s
  substitute m (DomainDeclaration i c es) = DomainDeclaration (substitute m i)
                                                            (substitute m c)
                                                            (substitute m es)
  substitute m (PortDeclaration p pt pc) = PortDeclaration p pt pc -- FIXME
  substitute m (Assignment i e) = Assignment (substitute i) (substitute m e)
  substitute m (PortConnection es c es') = PortConnection (substitute m es) c
                                                        (substitute m es')

instance Substitute Identifier Identifier Expression where
  substitute m (QualNameExpression n) = QualNameExpression (substitute m n)
  substitute m e@(IntExpression _) = e
  substitute m e@(StringExpression _) = e
  substitute m (ParenExpression e) = ParenExpression (substitute m e)

instance Substitute Identifier Identifier QualName where
  substitute m (UnQual n) = UnQual (substitute  m)
  substitute m (Qual n n') = Qual (substitute n) (substitute n')

instance

