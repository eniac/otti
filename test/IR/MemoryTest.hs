{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module IR.MemoryTest where
import           BenchUtils
import           Test.Tasty.HUnit
import qualified IR.Memory                     as Mem
import qualified IR.TySmt                      as Smt
import qualified Targets.SMT.Assert            as Assert

memoryTest :: BenchTest
memoryTest = benchTestGroup
  "Memory"
  [ benchTestCase "zero" $ do
    True @? "Good"
  , benchTestCase "init" $ do
    a <- Assert.evalAssert $ Mem.execMem Mem.initMem
    32 @=? Mem.pointerSize a
    Mem.Flat { Mem.blockSize = 32 } @=? Mem.memoryStrategy a
    1 @=? length (Mem.memories a)
  , benchTestCase "after next" $ do
    a <- Assert.evalAssert $ Mem.execMem $ do
      Mem.initMem
      Mem.nextMem
    32 @=? Mem.pointerSize a
    Mem.Flat { Mem.blockSize = 32 } @=? Mem.memoryStrategy a
    2 @=? length (Mem.memories a)
  , benchTestCase "assertions after next" $ do
    a <- Assert.execAssert $ Mem.execMem $ do
      Mem.initMem
      Mem.nextMem
    0 @=? length (Assert.asserted a)
  , benchTestCase "assertions after load" $ do
    a <- Assert.execAssert $ Mem.execMem $ do
      Mem.initMem
      Mem.memLoad (Mem.bvNum False 32 0) 1
    0 @=? length (Assert.asserted a)
  , benchTestCase "assertions after store" $ do
    a <- Assert.execAssert $ Mem.execMem $ do
      Mem.initMem
      Mem.memStore (Mem.bvNum False 32 0) (Mem.bvNum False 32 0) Nothing
    1 @=? length (Assert.asserted a)
    2 @=? sum (map countStores (Assert.asserted a))
  ]

countStores :: Smt.SortClass s => Smt.Term s -> Int
countStores = Smt.reduceTerm
  (\t -> case t of
    Smt.Store a i v -> Just $ 1 + countStores a + countStores i + countStores v
    _ -> Nothing
  )
  0
  (+)

