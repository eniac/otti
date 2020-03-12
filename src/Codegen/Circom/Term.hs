{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Codegen.Circom.Term ( lcZero
                           , Term(..)
                           , LTerm(..)
                           , Signal(..)
                           , WireBundle(..)
                           , Constraint
                           , BaseTerm
                           , LC
                           , mapSignalsInTerm
                           , mapVarsInTerm
                           , termSignals
                           , sigAsLinearTerm
                           , sigAsSigTerm
                           , sigAsWire
                           , sigAsLC
                           , sigAsSmt
                           , sigLocation
                           , Ctx(..)
                           , ctxStore
                           , ctxGet
                           , ctxAddConstraint
                           , ctxWithEnv
                           , ctxInit
                           , ctxToStruct
                           , ctxGetCallable
                           , ctxAddPublicSig
                           , ctxAddPrivateSig
                           ) where

import qualified AST.Circom                 as AST
import           Codegen.Circom.Constraints (Constraint, Constraints, LC,
                                             Signal)
import qualified Codegen.Circom.Constraints as CS
import           Data.Bifunctor
import qualified Data.Either                as Either
import           Data.Field.Galois          (Prime, PrimeField)
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import qualified Data.Maybe                 as Maybe
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           GHC.TypeLits
import qualified IR.TySmt                   as Smt

data WireBundle k = Sig Signal
                  | Scalar k
                  | Linear (LC k)
                  | Quadratic (LC k) (LC k) (LC k)
                  | Other
                  deriving (Show, Ord, Eq)

type BaseTerm k = (WireBundle (Prime k), Smt.Term (Smt.PfSort k))

data Term k = Base (BaseTerm k)
            | Array [Term k]                                    -- An array of terms
            | Struct (Ctx k)
            deriving (Show)

-- An evaluated l-value
data LTerm = LTermIdent String
           | LTermPin LTerm String
           | LTermIdx LTerm Int
           deriving (Show,Ord,Eq)

instance forall n. KnownNat n => Num (Smt.Term (Smt.PfSort n)) where
  a + b = Smt.PfNaryExpr Smt.PfAdd [a, b]
  a * b = Smt.PfNaryExpr Smt.PfMul [a, b]
  fromInteger i = Smt.IntToPf $ Smt.IntLit $ i `rem` natVal (Proxy :: Proxy n)
  signum s = Smt.IntToPf $ Smt.IntLit 1
  abs s = s
  negate = Smt.PfUnExpr Smt.PfNeg

instance forall k. KnownNat k => Fractional (Smt.Term (Smt.PfSort k)) where
  fromRational n = error "NYI"
  recip = Smt.PfUnExpr Smt.PfRecip

instance PrimeField k => Num (WireBundle k) where
  s + t = case (s, t) of
    (Other, _) -> Other
    (Sig s, r) -> sigAsWire s + r
    (Linear (m1, c1), Linear (m2, c2)) -> Linear (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Quadratic a b (m2, c2)) -> Quadratic a b (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Scalar c) -> Linear (m1, c1 + c)
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic a b (m, c1), Scalar c2) -> Quadratic a b (m, c1 + c2)
    (Scalar c1, Scalar c2) -> Scalar $ c1 + c2
    (l, r) -> r + l
  s * t = case (s, t) of
    (Other, _) -> Other
    (Sig s, r) -> sigAsWire s * r
    (Linear l1, Linear l2) -> Quadratic l1 l2 (Map.empty, 0)
    (Linear _, Quadratic {}) -> Other
    (Linear l, Scalar c) -> Linear $ lcScale l c
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic l1 l2 l3, Scalar c) -> Quadratic (lcScale l1 c) (lcScale l2 c) (lcScale l3 c)
    (Scalar c1 , Scalar c2) -> Scalar $ c1 * c2
    (l, r) -> r * l
  fromInteger n = Scalar $ fromInteger n
  signum s = case s of
    Sig {}       -> Scalar 1
    Other        -> Scalar 1
    Linear {}    -> Scalar 1
    Quadratic {} -> Scalar 1
    Scalar n     -> Scalar $ signum n
  abs s = case s of
    Other    -> Other
    Scalar n -> Scalar $ abs n
    _        -> s
  negate s = fromInteger (-1) * s

instance PrimeField k => Fractional (WireBundle k) where
  fromRational = Scalar . fromRational
  recip t = case t of
    Scalar c1    -> Scalar (recip c1)
    Other        -> Other
    Sig {}       -> Other
    Linear _     -> Other
    Quadratic {} -> Other

instance forall k. KnownNat k => Num (BaseTerm k) where
    (a, b) + (c, d) = (a + c, b + d)
    (a, b) * (c, d) = (a * c, b * d)
    fromInteger n = (fromInteger n, Smt.IntToPf $ Smt.IntLit n)
    signum (a, b) = (signum a, signum b)
    abs (a, b) = (abs a, abs b)
    negate (a, b) = (negate a, negate b)

instance forall k. KnownNat k => Fractional (BaseTerm k) where
    fromRational n = (fromRational n, fromRational n)
    recip (a, b) = (recip a, recip b)

instance forall k. KnownNat k => Num (Term k) where
  s + t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot add array term " ++ show a ++ " to anything"
    (a@Struct {}, _) -> error $ "Cannot add struct term " ++ show a ++ " to anything"
    (Base a, Base b) -> Base $ a + b
    (l, r) -> r + l
  s * t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot multiply array term " ++ show a ++ " with anything"
    (a@Struct {}, _) -> error $ "Cannot multiply struct term " ++ show a ++ " with anything"
    (Base a, Base b) -> Base $ a * b
    (l, r) -> r * l
  fromInteger n = Base $ fromInteger n
  signum s = case s of
    Array {}  -> error $ "Cannot get sign of array term " ++ show s
    Struct {} -> error $ "Cannot get sign of struct term " ++ show s
    Base a    -> Base $ signum a
  abs s = case s of
    Array a    -> Base $ fromIntegral $ length a
    Struct c   -> Base $ fromIntegral $ Map.size (env c)
    Base a     -> Base $ abs a
  negate s = fromInteger (-1) * s

instance forall k. KnownNat k => Fractional (Term k) where
  fromRational = Base . fromRational
  recip t = case t of
    a@Array {}  -> error $ "Cannot invert array term " ++ show a
    a@Struct {} -> error $ "Cannot invert struct term " ++ show a
    Base a      -> Base $ recip a

lcScale :: PrimeField k => LC k -> k -> LC k
lcScale (m, c) a = (Map.map (*a) m, a * c)

lcZero :: PrimeField k => LC k
lcZero = (Map.empty, 0)

sigAsLC :: PrimeField k => Signal -> LC k
sigAsLC s = (Map.fromList [(s, 1)], 0)

sigAsWire :: PrimeField k => Signal -> WireBundle k
sigAsWire = Linear . sigAsLC

sigAsSigTerm :: KnownNat k => Signal -> Term k
sigAsSigTerm s = Base (Sig s, sigAsSmt s)

sigAsLinearTerm :: KnownNat k => Signal -> Term k
sigAsLinearTerm s = Base (Linear (Map.fromList [(s, 1)], 0), sigAsSmt s)

sigAsSmt :: KnownNat n => Signal -> Smt.Term (Smt.PfSort n)
sigAsSmt = Smt.Var . show

mapSignalsInWires :: (Signal -> Signal) -> WireBundle k -> WireBundle k
mapSignalsInWires f t = case t of
    Sig s -> Sig $ f s
    Linear (m, c) -> Linear (Map.mapKeys f m, c)
    Quadratic (m1, c1) (m2, c2) (m3, c3) -> Quadratic (Map.mapKeys f m1, c1) (Map.mapKeys f m2, c2) (Map.mapKeys f m3, c3)
    Scalar c -> Scalar c
    Other -> Other

mapSignalsInTerm :: (Signal -> Signal) -> Term k -> Term k
mapSignalsInTerm f t = case t of
    Array ts -> Array $ map (mapSignalsInTerm f) ts
    Struct c -> Struct c { env = Map.map (mapSignalsInTerm f) $ env c
                         , constraints = CS.mapSignals f $ constraints c
                         -- Not needed, because we lift the map to the top context.
                         -- , numberToSignal = Map.map f $ numberToSignal c
                         }
    Base (a, b) -> Base (mapSignalsInWires f a, b)

mapVarsInTerm :: (String -> String) -> Term k -> Term k
mapVarsInTerm f t = case t of
    Array ts -> Array $ map (mapVarsInTerm f) ts
    Struct c -> Struct c { env = Map.map (mapVarsInTerm f) $ env c
                         }
    Base (a, b) -> Base (a, mapVarsInSmtTerm f b)

mapVarsInSmtTerm :: (String -> String) -> Smt.Term k -> Smt.Term k
mapVarsInSmtTerm f = Smt.mapTerm visit
  where
    visit :: Smt.Term t -> Maybe (Smt.Term t)
    visit t = case t of
      Smt.Var s -> Just $ Smt.Var $ f s
      _ -> Nothing


wireSignals :: WireBundle k -> Set Signal
wireSignals t = case t of
    Other -> Set.empty
    Sig s       -> Set.insert s Set.empty
    Linear (m, _) -> Set.fromList (Map.keys m)
    Quadratic (a, _) (b, _) (c, _) -> foldr Set.union Set.empty (map (Set.fromList . Map.keys) [a, b, c])
    Scalar s -> Set.empty

termSignals :: Term k -> Set Signal
termSignals t = case t of
    Array ts    -> foldr Set.union Set.empty $ map termSignals ts
    Struct c    -> foldr Set.union Set.empty $ map termSignals $ Map.elems $ env c
    Base (a, _) -> wireSignals a

sigLocation :: Signal -> LTerm
sigLocation s =
    foldl (flip $ either (flip LTermPin) (flip LTermIdx))
          (LTermIdent (CS.signalLeadingName s))
          (drop 1 $ CS.signalAccesses s)

data Ctx k = Ctx { env            :: Map.Map String (Term k)
                 , constraints    :: CS.Constraints (Prime k)
                 --                              (fn? , params  , body     )
                 -- NB: templates are not fn's.
                 --     functions are
                 , callables      :: Map.Map String (Bool, [String], AST.Block)
                 -- Must be prime.
                 , fieldOrder     :: Integer
                 , returning      :: Maybe (Term k)
                 -- During synthesis signals are all numbered.
                 , numberToSignal :: [(String, Signal)]
                 , nextSignal     :: Int
                 }
                 deriving (Show)

updateList :: (a -> a) -> Int -> [a] -> Maybe [a]
updateList f i l = case splitAt i l of
    (h, m:t) -> Just $ h ++ (f m : t)
    _        -> Nothing


ctxWithEnv :: KnownNat k => Map.Map String (Term k) -> Integer -> Ctx k
ctxWithEnv env order = Ctx
    { env = env
    , constraints = CS.empty
    , callables = Map.empty
    , fieldOrder = order
    , returning = Nothing
    , numberToSignal = []
    , nextSignal = 0
    }

assignTerm :: KnownNat k => Term k -> Term k -> Term k
assignTerm src dst = case (src, dst) of
    (Base (Sig s, _), Base (_, v)) -> Base (Sig s, v)
    (_, r)                         -> r

-- Modifies a context to store a value in a location
ctxStore :: KnownNat k => Ctx k -> LTerm -> Term k -> Ctx k
ctxStore ctx loc value = case value of
        -- No storing structures to foreign locations!
        -- Really, we want something stricter than this
        Struct ctx' -> if null (Either.lefts ss)
            then
                let
                    m' = Map.map (mapSignalsInTerm emmigrateSignal) $ env ctx'
                    c' = CS.mapSignals emmigrateSignal $ constraints ctx'
                    numberToSignal' = map (bimap id emmigrateSignal) (numberToSignal ctx')
                    s' = Struct ctx' { env = m', constraints = CS.empty, numberToSignal = [] }
                in
                    ctx { env = Map.update (pure . replacein ss s') ident $ env ctx
                        , constraints = CS.union c' $ constraints ctx
                        , numberToSignal = numberToSignal' ++ numberToSignal ctx
                        }
            else
                error $ "Cannot assign circuits to non-local location: " ++ show loc
        _ -> ctx { env = Map.update (pure . replacein ss value) ident (env ctx) }
    where
        (ident, ss) = steps loc
        emmigrateSignal = CS.SigForeign ident (Either.rights ss)

        -- Given
        --   * a location as a list of pins/indices, ([Either String Int])
        --   * a value (Term)
        --   * a term (Term)
        -- locates the sub-term of term at the location (first pin/idx applies
        -- to the top of the term) and replaces it with `value`.
        replacein :: KnownNat k => [Either String Int] -> Term k -> Term k -> Term k
        replacein location value term = case location of
            [] -> assignTerm term value
            Left pin:rest -> case term of
                Struct c -> Struct c { env = Map.update (pure . replacein rest value) pin (env c) }
                _ -> error $ "Cannot update pin `" ++ pin ++ "` of non-struct " ++ show term
            Right idx:rest -> case term of
                Array m -> Array $ Maybe.fromMaybe (error $ "The index " ++ show idx ++ " is out of bounds for " ++ show m)
                                                   (updateList (replacein rest value) idx m)
                _ -> error $ "Cannot update index `" ++ show idx ++ "` of non-array " ++ show term

        -- Extract the steps of the access
        -- first the identifier
        -- second the pins/indices to follows, tail-first.
        rsteps (LTermIdent s) = (s, [])
        rsteps (LTermPin lt pin) = let (s, ts) = rsteps lt in (s, Left pin:ts)
        rsteps (LTermIdx lt idx) = let (s, ts) = rsteps lt in (s, Right idx:ts)

        steps l = let (s, ts) = rsteps l in (s, reverse ts)


-- Gets a value from a location in a context
ctxGet :: KnownNat k => Ctx k -> LTerm -> Term k
ctxGet ctx loc = case loc of
    LTermIdent s -> r
        where r = Map.findWithDefault (error $ "Unknown identifier `" ++ s ++ "`") s (env ctx)
    LTermPin loc' pin -> case ctxGet ctx loc' of
        Struct c -> (env c) Map.! pin
        l -> error $ "Non-struct " ++ show l ++ " as location in " ++ show loc
    LTermIdx loc' i -> case ctxGet ctx loc' of
        Array ts -> if i < length ts then ts !! i else error $ "Idx " ++ show i ++ " too big for " ++ show ts
        l -> error $ "Non-array " ++ show l ++ " as location in " ++ show loc

ctxInit :: KnownNat k => Ctx k -> String -> Term k -> Ctx k
ctxInit ctx name value = ctx { env = Map.insert name value (env ctx) }

-- Given a context and an identifier too find, looks up the callable (function or template) of that name.
-- Returns the (whether it is a function, the formal parameters, the body)
ctxGetCallable :: KnownNat k => Ctx k -> String -> (Bool, [String], AST.Block)
ctxGetCallable ctx name = Maybe.fromMaybe (error $ "No template named " ++ name ++ " found") $ Map.lookup name (callables ctx)

ctxAddConstraint ctx c = ctx { constraints = CS.addEquality c $ constraints ctx }

ctxAddPublicSig :: Signal -> Ctx k -> Ctx k
ctxAddPublicSig s c = c { constraints = CS.addPublic s $ constraints c }

ctxAddPrivateSig :: Signal -> Ctx k -> Ctx k
ctxAddPrivateSig s c = c { constraints = CS.addPrivate s $ constraints c }

ctxToStruct :: Ctx k -> Term k
ctxToStruct ctx = Struct ctx
