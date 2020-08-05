{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Codegen.C.CToR1cs
  ( transFn
  , emitFnAsR1cs
  , FnTrans
  )
where

import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.Assert                 as Assert
import qualified Language.C.Syntax.AST         as AST
import           Codegen.C.CompilerMonad        ( evalCodegen, initValues )
import           Codegen.Circom.CompTypes.LowDeg
                                                ( LC
                                                , QEQ
                                                )
import qualified IR.R1cs.Opt                    as Opt
import           Codegen.C                      ( codegenFn )
import           IR.SMT.ToPf                    ( toPf )
import           IR.SMT.Opt                     ( constantFold
                                                , eqElim
                                                )
import           IR.R1cs                        ( sigMapQeq
                                                , qeqToR1csLines
                                                , R1CS(..)
                                                , writeToR1csFile
                                                , r1csStats
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
    Assert.runAssert $ evalCodegen False $ initValues >> codegenFn tu name
  return $ FnTrans { assertions = Assert.asserted assertState
                   , inputs     = inputs
                   , output     = output
                   }

emitFnAsR1cs
  :: forall n . KnownNat n => AST.CTranslUnit -> String -> FilePath -> IO ()
emitFnAsR1cs tu fnName path = do
  fn <- transFn tu fnName
  let pubVars = Set.insert (output fn) $ Set.fromList $ inputs fn
  -- TODO: Use R1CS for optimization
  r <- toPf @n Nothing pubVars $ eqElim pubVars $ map constantFold $ assertions fn
  putStrLn $ r1csStats r
  let r' = Opt.opt r
  putStrLn $ r1csStats r'
  writeToR1csFile r' path
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
