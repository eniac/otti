module IR.SMT.MemRouteTest
  ( test_benesRoute
  , test_benesRoute3
  )
where
import           IR.SMT.Opt.Mem.Route
import qualified Data.List                     as DLs
import qualified Test.QuickCheck               as QC

-- check that Benes is OK
test_benesRoute_ :: Int -> QC.Gen Bool
test_benesRoute_ len = do
  let ivals = take (4 + len `mod` 1024) [0 ..]
  ovals <- QC.shuffle ivals
  let (sw_i, sw_o) = benesRoute ivals ovals
      (it  , ib  ) = benesTopBottom sw_i ivals
      (ot  , ob  ) = benesTopBottom sw_o ovals
      imatch       = it == DLs.sort ot
      omatch       = ib == DLs.sort ob
  return $ imatch && omatch

test_benesRoute = QC.withMaxSuccess 16 test_benesRoute_

-- check benes3
test_benesRoute3_ :: Int -> QC.Gen Bool
test_benesRoute3_ st = do
  let ivals = take 3 [st ..]
  ovals <- QC.shuffle ivals
  let [sw_i, sw_m, sw_o] = benesRoute3 ivals ovals
      [i0  , i1  , i2  ] = ivals
      (t0, m )           = if sw_i then (i1, i0) else (i0, i1)
      (t1, o2)           = if sw_m then (i2, m) else (m, i2)
      (o0, o1)           = if sw_o then (t1, t0) else (t0, t1)
  return $ ovals == [o0, o1, o2]

test_benesRoute3 = QC.withMaxSuccess 16 test_benesRoute3_
