{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Codegen.C.CUtilsTest where
import qualified AST.C                  as AST
import           BenchUtils
import           Codegen.C.CUtils
import qualified Codegen.Circify.Memory as Mem
import qualified Data.Map.Strict        as M
import qualified IR.SMT.Assert          as Assert
import qualified IR.SMT.TySmt           as Ty
import           Test.Tasty.HUnit

cutilsTest :: BenchTest
cutilsTest = benchTestGroup
  "CUtils"
  [ benchTestCase "new var" $ do
    a <- Assert.execAssert $ Mem.execMem $ do
      Mem.initMem
      cDeclVar True AST.U8 "my_u8"
    (2 + 1) @=? M.size (Assert.vars a)
  , benchTestCase "new vars" $ do
    a <- Assert.execAssert $ Mem.execMem $ do
      Mem.initMem
      _ <- cDeclVar True AST.U8 "my_u8"
      cDeclVar True AST.S8 "my_i8"
    (2 + 2 + 1) @=? M.size (Assert.vars a)
  , benchTestCase "cAdd: u8 + i8 = i8" $ do
    a <- Assert.evalAssert $ Mem.evalMem $ do
      Mem.initMem
      u <- cDeclVar True AST.U8 "my_u8"
      i <- cDeclVar True AST.S8 "my_i8"
      return $ cAdd u i
    AST.S8 @=? cType a
  , benchTestCase "cAdd: i32 + i8 = i32" $ do
    a <- Assert.evalAssert $ Mem.evalMem $ do
      Mem.initMem
      u <- cDeclVar True AST.S32 "my_i32"
      i <- cDeclVar True AST.S8 "my_i8"
      return $ cAdd u i
    AST.S32 @=? cType a
    let (_, w, bv) = asInt $ term a
    Ty.SortBv w @=? Ty.sort bv
  , benchTestCase "cNeg: -u8 = i8" $ do
    a <- Assert.evalAssert $ Mem.evalMem $ do
      Mem.initMem
      u <- cDeclVar True AST.U8 "my_u8"
      return $ cNeg u
    AST.S8 @=? cType a
  , benchTestCase "cNot: !u8 = bool" $ do
    a <- Assert.evalAssert $ Mem.evalMem $ do
      Mem.initMem
      u <- cDeclVar True AST.U8 "my_u8"
      return $ cNot u
    AST.Bool @=? cType a
  , benchTestCase "cCond: bool ? u8 : i8 = u8" $ do
    a <- Assert.evalAssert $ Mem.evalMem $ do
      Mem.initMem
      u <- cDeclVar True AST.U8 "my_u8"
      i <- cDeclVar True AST.S8 "my_i8"
      b <- cDeclVar True AST.Bool "my_bool"
      return $ cCond b u i
    AST.U8 @=? cType a
  --, benchTestCase "cStore + cLoad preserves type and size of u8" $ do
  --  a <- Assert.evalAssert $ Mem.evalMem $ do
  --    Mem.initMem
  --    u <- cDeclVar True AST.U8 "my_u8"
  --    p <- cDeclVar True (AST.Ptr32 AST.U8) "my_u8_ptr"
  --    _ <- Mem.liftAssert $ cAssign True p (cIntLit AST.U32 0)
  --    _ <- cStore p u (Ty.BoolLit True)
  --    snd <$> cLoad p
  --  let (_, w, bv) = asInt $ term a
  --  Ty.SortBv w @=? Ty.sort bv
  --  AST.U8 @=? cType a
  --, benchTestCase "cStore + cLoad preserves type and size of *u8" $ do
  --  a <- Assert.evalAssert $ Mem.evalMem $ do
  --    Mem.initMem
  --    u <- cDeclVar True (AST.Ptr32 AST.U8) "my_u8_ptr"
  --    p <- cDeclVar True (AST.Ptr32 (AST.Ptr32 AST.U8)) "my_u8_ptr_ptr"
  --    _ <- Mem.liftAssert $ cAssign True p (cIntLit AST.U32 0)
  --    _ <- cStore p u (Ty.BoolLit True)
  --    cLoad p
  --  Ty.SortBv 32 @=? Ty.sort (snd $ asStaticPtr $ term a)
  --  (AST.Ptr32 AST.U8) @=? cType a
  ]
