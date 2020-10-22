{-# LANGUAGE GADTs #-}
module Codegen.C.TermTest where
import qualified Codegen.C.Type                as Type
import           BenchUtils
import           Codegen.C.Term
import qualified Codegen.Circify.Memory        as Mem
import qualified Data.Map.Strict               as M
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.TySmt                  as Ty
import           Test.Tasty.HUnit
import           Util.Cfg                       ( evalCfgDefault )

declVar b c = cDeclVar Nothing True b c Nothing

cutilsTest :: BenchTest
cutilsTest = benchTestGroup
  "Term"
  [ benchTestCase "new var" $ do
    a <- evalCfgDefault $ Assert.execAssert $ Mem.execMem $ declVar Type.U8
                                                                    "my_u8"
    2 @=? M.size (Assert.vars a)
  , benchTestCase "new vars" $ do
    a <- evalCfgDefault $ Assert.execAssert $ Mem.execMem $ do
      _ <- declVar Type.U8 "my_u8"
      declVar Type.S8 "my_i8"
    (2 + 2) @=? M.size (Assert.vars a)
  , benchTestCase "cAdd: u8 + i8 = int" $ do
    a <- evalCfgDefault $ Assert.evalAssert $ Mem.evalMem $ do
      u <- declVar Type.U8 "my_u8"
      i <- declVar Type.S8 "my_i8"
      return $ cAdd u i
    Type.S32 @=? cType a
  , benchTestCase "cAdd: i32 + i8 = i32" $ do
    a <- evalCfgDefault $ Assert.evalAssert $ Mem.evalMem $ do
      u <- declVar Type.S32 "my_i32"
      i <- declVar Type.S8 "my_i8"
      return $ cAdd u i
    Type.S32 @=? cType a
    let (_, w, bv) = asInt $ term a
    Ty.SortBv w @=? Ty.sort bv
  , benchTestCase "cNeg: -u8 = i8" $ do
    a <- evalCfgDefault $ Assert.evalAssert $ Mem.evalMem $ do
      u <- declVar Type.U8 "my_u8"
      return $ cNeg u
    Type.S8 @=? cType a
  , benchTestCase "cNot: !u8 = bool" $ do
    a <- evalCfgDefault $ Assert.evalAssert $ Mem.evalMem $ do
      u <- declVar Type.U8 "my_u8"
      return $ cNot u
    Type.Bool @=? cType a
  , benchTestCase "cCond: bool ? u8 : i8 = u8" $ do
    a <- evalCfgDefault $ Assert.evalAssert $ Mem.evalMem $ do
      u <- declVar Type.U8 "my_u8"
      i <- declVar Type.S8 "my_i8"
      b <- declVar Type.Bool "my_bool"
      return $ cCond b u i
    Type.U8 @=? cType a
  --, benchTestCase "cStore + cLoad preserves type and size of u8" $ do
  --  a <- Assert.evalAssert $ Mem.evalMem $ do
  --    u <- declVar Type.U8 "my_u8"
  --    p <- declVar (Type.Ptr32 Type.U8) "my_u8_ptr"
  --    _ <- Mem.liftAssert $ cAssign p (cIntLit Type.U32 0)
  --    _ <- cStore p u (Ty.BoolLit )
  --    snd <$> cLoad p
  --  let (_, w, bv) = asInt $ term a
  --  Ty.SortBv w @=? Ty.sort bv
  --  Type.U8 @=? cType a
  --, benchTestCase "cStore + cLoad preserves type and size of *u8" $ do
  --  a <- Assert.evalAssert $ Mem.evalMem $ do
  --    u <- declVar (Type.Ptr32 Type.U8) "my_u8_ptr"
  --    p <- declVar (Type.Ptr32 (Type.Ptr32 Type.U8)) "my_u8_ptr_ptr"
  --    _ <- Mem.liftAssert $ cAssign True p (cIntLit Type.U32 0)
  --    _ <- cStore p u (Ty.BoolLit True)
  --    cLoad p
  --  Ty.SortBv 32 @=? Ty.sort (snd $ asStaticPtr $ term a)
  --  (Type.Ptr32 Type.U8) @=? cType a
  ]
