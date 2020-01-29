{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Codegen.Circom.Context ( Ctx(..)
                              , ctxStore
                              , ctxGet
                              , ctxAddConstraint
                              , ctxWithEnv
                              , ctxInit
                              , ctxToStruct
                              , ctxGetTemplate
                              ) where

import qualified Data.Maybe      as Maybe
import qualified Data.Either     as Either
import qualified Data.Map.Strict as Map
import           Codegen.Circom.Term
import qualified AST.Circom      as AST

data Ctx = Ctx { env         :: Map.Map String Term
                       , constraints :: [Constraint]
                       , templates   :: Map.Map String ([String], AST.Block)
                       , fieldOrder  :: Integer                                 -- Must be a prime
                       }
                       deriving (Show, Eq)

updateList :: (a -> a) -> Int -> [a] -> Maybe [a]
updateList f i l = case splitAt i l of
    (h, m:t) -> Just $ h ++ (f m : t)
    _        -> Nothing


ctxWithEnv :: Map.Map String Term -> Integer -> Ctx
ctxWithEnv env order = Ctx { env = env, constraints = [], templates = Map.empty , fieldOrder = order}

-- Modifies a context to store a value in a location
ctxStore :: Ctx -> LTerm -> Term -> Ctx
ctxStore ctx loc value = case value of
        -- No storing structures to foreign locations!
        -- Really, we want something stricter than this
        Struct m c -> if null (Either.lefts ss)
            then
                let
                    m' = emmigrateMap m
                    c' = emmigrateConstraints c
                in
                    ctx { env = Map.update (pure . replacein ss (Struct m' c')) ident (env ctx)
                        , constraints = c' ++ constraints ctx }
            else
                error $ "Cannot assign circuits to non-local location: " ++ show loc
        _ -> ctx { env = Map.update (pure . replacein ss value) ident (env ctx) }
    where
        (ident, ss) = steps loc

        emmigrateMap = Map.map (signalTranform emmigrateSignal)
        emmigrateConstraints = map (\(a, b, c) -> (emmigrateLC a, emmigrateLC b, emmigrateLC c))
        emmigrateLC (m, c) = (Map.mapKeys emmigrateSignal m, c)
        emmigrateSignal = SigForeign ident (Either.rights ss)

        -- Given
        --   * a location as a list of pins/indices, ([Either String Int])
        --   * a value (Term)
        --   * a term (Term)
        -- locates the sub-term of term at the location (first pin/idx applies
        -- to the top of the term) and replaces it with `value`.
        replacein :: [Either String Int] -> Term -> Term -> Term
        replacein location value term = case location of
            [] -> value
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
ctxGet :: Ctx -> LTerm -> Term
ctxGet ctx loc = case loc of
    LTermIdent s -> Map.findWithDefault (error $ "Unknown identifier `" ++ s ++ "`") s (env ctx)
    LTermPin loc' pin -> case ctxGet ctx loc' of
        Struct pins _ -> pins Map.! pin
        l -> error $ "Non-struct " ++ show l ++ " as location in " ++ show loc
    LTermIdx loc' i -> case ctxGet ctx loc' of
        Array ts -> ts !! i
        l -> error $ "Non-array " ++ show l ++ " as location in " ++ show loc

ctxInit :: Ctx -> String -> Term -> Ctx
ctxInit ctx name value = ctx { env = Map.insert name value (env ctx) }

ctxGetTemplate :: Ctx -> String -> ([String], AST.Block)
ctxGetTemplate ctx name = Maybe.fromMaybe (error $ "No template named " ++ name ++ " found") $ Map.lookup name (templates ctx)

ctxAddConstraint :: Ctx -> (LC, LC, LC) -> Ctx
ctxAddConstraint ctx c = ctx { constraints = c : constraints ctx }

ctxToStruct ctx = Struct (env ctx) (constraints ctx)
