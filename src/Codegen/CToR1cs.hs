{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Codegen.CToR1cs
  ( transFn, emitFnAsR1cs, FnTrans )
where

import qualified IR.TySmt                      as Ty
import qualified Targets.SMT.Assert            as Assert
import qualified Language.C.Syntax.AST         as AST
import           Codegen.CompilerMonad          ( evalCodegen )
import           Codegen.Circom.CompTypes.LowDeg
                                                ( LC
                                                , QEQ
                                                )
import           Codegen.C                      ( codegenFn )
import           Codegen.ToPf                   ( toPf )
import           Codegen.Opt                    ( constantFold
                                                , eqElim
                                                )
import           Codegen.Circom.R1cs            ( sigMapQeq
                                                , qeqToR1csLines
                                                )
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           GHC.TypeNats                   ( KnownNat )
import           Data.Field.Galois              ( Prime
                                                , toP
                                                )

data FnTrans = FnTrans { assertions :: [Ty.TermBool]
                       , inputs :: [String]
                       , output :: String
                       }

-- Can a fn exhibit undefined behavior?
-- Returns a string describing it, if so.
transFn :: AST.CTranslUnit -> String -> IO FnTrans
transFn tu name = do
  ((inputs, output), assertState) <-
    Assert.runAssert $ evalCodegen False $ codegenFn tu name
  return $ FnTrans { assertions = Assert.asserted assertState
                   , inputs     = inputs
                   , output     = output
                   }

emitFnAsR1cs
  :: forall n . KnownNat n => AST.CTranslUnit -> String -> FilePath -> IO ()
emitFnAsR1cs tu fnName path = do
  fn <- transFn tu fnName
  let pubVars = Set.insert (output fn) $ Set.fromList $ inputs fn
  r <- toPf @n $ eqElim pubVars $ map constantFold $ assertions fn
  let otherVars =
        Set.difference (foldl1 Set.union $ map collectVarsQeq r) pubVars
  let vMap = Map.fromList
        $ zip (Set.toAscList pubVars ++ Set.toAscList otherVars) [0 ..]
  let qeqs = map (sigMapQeq (vMap Map.!)) r
  let ls =
        [ fromIntegral (Set.size pubVars)
          , fromIntegral (Set.size otherVars)
          , fromIntegral (length qeqs)
          ]
          : concatMap qeqToR1csLines qeqs
  writeFile path $ unlines $ map (unwords . map show) ls
  return ()
 where
  collectVarsLc :: LC String (Prime n) -> Set.Set String
  collectVarsLc = Set.fromList . Map.keys . fst
  collectVarsQeq :: QEQ String (Prime n) -> Set.Set String
  collectVarsQeq (a, b, c) =
    Set.union (Set.union (collectVarsLc a) (collectVarsLc b)) (collectVarsLc c)
  collectVars :: Ty.SortClass s => Ty.Term s -> Set.Set String
  collectVars = Ty.reduceTerm visit Set.empty Set.union
   where
    visit :: Ty.SortClass s => Ty.Term s -> Maybe (Set.Set String)
    visit t = case t of
      Ty.Var n _ -> Just $ Set.singleton n
      _          -> Nothing
