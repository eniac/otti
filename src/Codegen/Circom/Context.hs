{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Codegen.Circom.Context ( Ctx(..)
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

import qualified IR.TySmt                   as Smt
import qualified AST.Circom                 as AST
import           Codegen.Circom.Constraints (Constraint, LC, Signal)
import qualified Codegen.Circom.Constraints as CS
import           Codegen.Circom.Term
import qualified Data.Either                as Either
import           Data.Field.Galois          (Prime)
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import qualified Data.Maybe                 as Maybe
import           Debug.Trace                (trace)
import           GHC.TypeLits               (KnownNat, natVal)

data Ctx k = Ctx { env         :: Map.Map String (Term k)
                 , constraints :: CS.Constraints (Prime k)
                 --                              (fn? , params  , body     )
                 -- NB: templates are not fn's.
                 --     functions are
                 , callables   :: Map.Map String (Bool, [String], AST.Block)
                 -- Must be prime.
                 , fieldOrder  :: Integer
                 , returning   :: Maybe (Term k)
                 }
                 deriving (Show)

updateList :: (a -> a) -> Int -> [a] -> Maybe [a]
updateList f i l = case splitAt i l of
    (h, m:t) -> Just $ h ++ (f m : t)
    _        -> Nothing


ctxWithEnv :: KnownNat k => Map.Map String (Term k) -> Integer -> Ctx k
ctxWithEnv env order = Ctx { env = env, constraints = CS.empty, callables = Map.empty , fieldOrder = order, returning = Nothing }

assignTerm :: KnownNat k => Term k -> Term k -> Term k
assignTerm src dst = case (src, dst) of
    (Base (Sig s, _), Base (_, v)) -> Base (Sig s, v)
    (_, r)                         -> r


-- Modifies a context to store a value in a location
ctxStore :: KnownNat k => Ctx k -> LTerm -> Term k -> Ctx k
ctxStore ctx loc value = case value of
        -- No storing structures to foreign locations!
        -- Really, we want something stricter than this
        Struct m c -> if null (Either.lefts ss)
            then
                let
                    m' = Map.map (mapSignalsInTerm emmigrateSignal emmigrateSignalString) m
                    c' = CS.mapSignals emmigrateSignal c
                in
                    ctx { env = Map.update (pure . replacein ss (Struct m' c')) ident (env ctx)
                        , constraints = CS.union c' (constraints ctx) }
            else
                error $ "Cannot assign circuits to non-local location: " ++ show loc
        _ -> ctx { env = Map.update (pure . replacein ss value) ident (env ctx) }
    where
        (ident, ss) = steps loc
        emmigrateSignal = SigForeign ident (Either.rights ss)
        emmigrateSignalString s = intercalate "." (ident:map (\i -> "[" ++ show i ++ "]") (Either.rights ss)) ++ "." ++ s

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
                Struct m c -> Struct (Map.update (pure . replacein rest value) pin m) c
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
        Struct pins _ -> pins Map.! pin
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

ctxToStruct ctx = Struct (env ctx) (constraints ctx)
