{-
 - arbitrary-sized Waksman network switch setting implementation
 -}

module IR.SMT.Opt.Mem.Route
  ( benesRoute
  , benesRoute3
  , benesTopBottom
  )
where

import qualified Control.Monad.ST              as CmST
import qualified Data.Array.ST                 as DaST
import           Data.Bits                      ( xor )
import qualified Data.IntSet                   as DISet
import qualified Data.STRef                    as DStR

import           Control.Monad                  ( liftM
                                                , forM_
                                                , when
                                                )
import qualified Data.List                     as DLs
import qualified Data.Map.Strict               as DM
import           Data.Maybe                     ( fromJust )

-- precondition: ins is sorted, outs is a permutation of ins
-- returns the indices
toPermOrder :: [Int] -> [Int] -> [Int]
toPermOrder ins outs = res
 where
  iomap = DM.fromDistinctAscList $ zip ins [0 ..]
  res   = map ((DM.!) iomap) outs

-- routing for the case of 3 inputs
benesRoute3 :: [Int] -> [Int] -> [Bool]
benesRoute3 inp outp = [sw_i, sw_m, sw_o]
 where
  [ord0, ord1, ord2] = toPermOrder inp outp
  (sw_m, sw_i, sw_o) = if ord2 /= 2
    then (True, ord2 == 0, ord1 /= 2)
    else (False, False, ord0 == 1)

-- given a set of switches, split input values into top/bottom lists
benesTopBottom :: [Bool] -> [Int] -> ([Int], [Int])
benesTopBottom sws vals = (reverse i, reverse o)
 where
  bTBHelp (top, bot) [] []           = (top, bot)
  bTBHelp (top, bot) [] (v     : []) = (top, v : bot)
  bTBHelp (top, bot) [] (v : w : []) = (v : top, w : bot)
  bTBHelp (top, bot) (s : sw) (v : w : xs) =
    let (tt, bb) = if s then (w : top, v : bot) else (v : top, w : bot)
    in  bTBHelp (tt, bb) sw xs
  bTBHelp _ _ _ = undefined -- illegal: ran out of sws or vals
  (i, o) = bTBHelp ([], []) sws vals

-- Given:
--   * inp: a list [0..(n-1)]
--   * outp: a permutation of that list
-- Returns:
--   * the crossbar switch settings for the initial and final layer
-- precondition: outp is permutation of inp, inp is sorted
benesRoute :: [Int] -> [Int] -> ([Bool], [Bool])
benesRoute inp outp = CmST.runST $ do
    -- misc values
  let num_inputs  = length inp
      is_odd      = num_inputs `mod` 2 == 1
      is_odd_     = if is_odd then 1 else 0
      iolen       = num_inputs `div` 2
      swnum_o_max = iolen - 1 + is_odd_

  -- input and output switch settings
  sw_i <-
    DaST.newArray (0, iolen - 1) Nothing :: CmST.ST
      s
      (DaST.STArray s Int (Maybe Bool))
  sw_o <-
    DaST.newArray (0, iolen - 2 + is_odd_) Nothing :: CmST.ST
      s
      (DaST.STArray s Int (Maybe Bool))

  -- compute output ordering (maps output idx to input idx)
  let out_ord_ = toPermOrder inp outp
  out_ord <-
    DaST.newListArray (0, num_inputs - 1) out_ord_ :: CmST.ST
      s
      (DaST.STUArray s Int Int)
  -- compute reverse map (maps input idx to output idx)
  rev_map <-
    DaST.newArray_ (0, num_inputs - 1) :: CmST.ST s (DaST.STUArray s Int Int)
  forM_ (zip out_ord_ [0 ..]) $ \(val, idx) -> do
    DaST.writeArray rev_map val idx

  -- queue: [ (input_idx, is_top) ]
  -- XXX(rsw) is there a better way to do this? Really, need a STSet type...
  invals <- DStR.newSTRef . DISet.fromDistinctAscList $ take num_inputs [0 ..]
  queue  <- if is_odd
    then do
      outl1 <- DaST.readArray out_ord (num_inputs - 1)
      DStR.modifySTRef' invals $ DISet.delete outl1
      DStR.modifySTRef' invals $ DISet.delete (num_inputs - 1)
      return [(num_inputs - 1, False), (outl1, False)]
    else do
      outl1 <- DaST.readArray out_ord (num_inputs - 1)
      DStR.modifySTRef' invals $ DISet.delete outl1
      outl2 <- DaST.readArray out_ord (num_inputs - 2)
      DStR.modifySTRef' invals $ DISet.delete outl2
      return [(outl2, True), (outl1, False)]

  -- routing algorithm
  let
    route_loop queue = do
        -- figure out next queue element to work on, or take one from invals, or we're done
      let qu = DLs.uncons queue
      invals_empty  <- liftM DISet.null $ DStR.readSTRef invals
      (elm, queue') <- case (qu, invals_empty) of
        (Nothing, True ) -> return (Nothing, [])
        (Nothing, False) -> do
          inval_next <- liftM DISet.findMin $ DStR.readSTRef invals
          -- XXX need to delete from inval here
          return (Just (inval_next, inval_next `mod` 2 == 0), [])
        (Just (h, t), _) -> return (Just h, t)

      when (elm /= Nothing) $ do
          -- input-side switch constraints
        let Just (idx_i, top_i) = elm
            swnum_i             = idx_i `div` 2
            swval_i             = (idx_i `mod` 2 == 0) /= top_i

        -- input-side switch
        let swnum_i_lim = if swnum_i >= iolen then 0 else swnum_i
        sw_i_val <- DaST.readArray sw_i swnum_i_lim
        if swnum_i >= iolen || sw_i_val /= Nothing
          then route_loop queue'  -- next iteration
          else do
            DaST.writeArray sw_i swnum_i $! Just swval_i

            -- output-side switch constraints
            let nidx_i = idx_i `xor` 1
            DStR.modifySTRef' invals $ DISet.delete nidx_i
            idx_o <- DaST.readArray rev_map nidx_i
            let swnum_o = idx_o `div` 2
                top_o   = not top_i
                swval_o = (idx_o `mod` 2 == 0) /= top_o

            -- output-side switch
            let swnum_o_lim = if swnum_o == swnum_o_max then 0 else swnum_o
            sw_o_val <- DaST.readArray sw_o swnum_o_lim
            if swnum_o == swnum_o_max || sw_o_val /= Nothing
              then route_loop queue'
              else do
                DaST.writeArray sw_o swnum_o $! Just swval_o

                -- new constraint?
                let nidx_o = idx_o `xor` 1
                nidx_o_in    <- DaST.readArray out_ord nidx_o
                update_queue <- liftM (DISet.member nidx_o_in)
                  $ DStR.readSTRef invals
                if not update_queue
                  then route_loop queue'
                  else do
                    DStR.modifySTRef' invals $ DISet.delete nidx_o_in
                    route_loop $ (nidx_o_in, not top_o) : queue'

  -- kick off routing
  route_loop queue
  -- return the result
  sw_i_ls <- map fromJust <$> DaST.getElems sw_i
  sw_o_ls <- map fromJust <$> DaST.getElems sw_o
  return (sw_i_ls, sw_o_ls)
