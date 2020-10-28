{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module IR.Circify.LoopFlattening
  ( loopFlatten
  )
where
import           Control.Monad.Reader           ( asks )
import           Control.Monad.State.Strict
import           Util.Cfg
import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as M
import           Numeric.Natural
import           IR.Circify.Control

-- Break a program containing loops into sections pre-loop, loop and post-loop
data LoopExpansion t = LoopExpansion (Control t) (Control t) (Control t)

instance Semigroup (LoopExpansion t) where
    -- No loop
  LoopExpansion p Empty e <> LoopExpansion p' Empty e' =
    LoopExpansion (p <> e <> p' <> e') mempty mempty
  -- Loop on the left
  LoopExpansion p Empty e <> LoopExpansion p' l e' =
    LoopExpansion (p <> e <> p') l e'
  -- Loop on the right
  LoopExpansion p l e <> LoopExpansion p' l' e' =
    LoopExpansion p l (e <> p' <> l' <> e')

-- Take some program and expand the first loop
-- Invariant: First loop found is in loop,
-- everything before it in `pre` and everything after it (including other loops) in `post`
pullbackLoop :: Control t -> LoopExpansion t
pullbackLoop l@(While _ _     ) = LoopExpansion Empty l Empty
pullbackLoop l@(For _ _ _ _   ) = LoopExpansion Empty l Empty
pullbackLoop (Seq left right  ) = pullbackLoop left <> pullbackLoop right
pullbackLoop o                  = LoopExpansion o Empty Empty

-- Need a context for variable shadowing and a Cfg for arguments
type LoopAnalysis m = StateT (Map String Natural) Cfg m

mkLoop
  :: (ControlTerm t, Monad m)
  => t
  -> t
  -> t
  -> Control t
  -> Control t
  -> Control t
  -> Control t
  -> StateT (Map String Natural) m (Control t)
mkLoop maxIteration outterCond innerCond prologue body1 body2 body3 = do
    -- Need different versions of these to avoid shadowing
  dummy <- var <$> newIdentifier "dummy"
  state <- var <$> newIdentifier "state"
  return $ state =: 0 <> prologue <>
    For dummy (lit 0) maxIteration (
      If (state ==: lit 0) (
        If (outterCond) (
            body1
            <> state =: 1
        ) (state =: 3)) Empty
    <> If (state ==: lit 1) (
        If (innerCond) (
            body2
        ) (state =: 2)) Empty
    <> If (state ==: lit 2) (
        body3
        <> state =: 0) Empty
    )
 where
  newIdentifier :: Monad m => String -> StateT (Map String Natural) m String
  newIdentifier identifier = do
    dict <- get
    case M.insertLookupWithKey (const (+)) identifier 1 dict of
      (Nothing, newdict) -> do
        put newdict
        return identifier
      (Just v, newdict) -> do
        put newdict
        return $ identifier ++ (show v)

-- Module entry point, flattens a loop given some configuration variable maxIterations
loopFlatten :: ControlTerm t => Control t -> Cfg (Control t)
loopFlatten top = evalStateT (loopFlatten' top) M.empty

-- Main loop flattening transformation, flattens loop-in-loop into one top level loop
--   Wahby et al. "Efficient RAM and control flow in verifiable outsourced computation"
loopFlatten' :: ControlTerm t => Control t -> LoopAnalysis (Control t)
loopFlatten' top = do
  maxIteration <- asks (Util.Cfg._loopMaxIteration)
  case (pullbackLoop top) of
    (LoopExpansion op (While cond body) ep) -> -- Top level (While)
                                               case (pullbackLoop body) of
      (LoopExpansion body1 (For i _ m body2) body3) ->  -- Inner loop (For)
                                                       do
        flatbody2 <- loopFlatten' body2
        flatbody3 <- loopFlatten' body3
        flatep    <- loopFlatten' ep
        flatloop  <- mkLoop (lit . toInteger $ maxIteration) -- Maximum iteration used for while loops as upper-bound
                            cond                             -- Outer loop conditional
                            (i <: m)                         -- Inner loop conditional (for loop, (i < m))
                            (i =: 0)                         -- Prologue statements (allocate i in the stack)
                            body1                            -- Outer loop body, before inner loop starts
                            (flatbody2 <> (i ++:))           -- Inner loop body (for lopp, do i++)
                            flatbody3                        -- Outer loop body, after inner loop returns
        return $ op <> flatloop <> flatep
      (LoopExpansion body1 (While condInner body2) body3) -> -- Inner loop (While)
                                                             do
        flatbody2 <- loopFlatten' body2
        flatbody3 <- loopFlatten' body3
        flatep    <- loopFlatten' ep
        flatloop  <- mkLoop (lit . toInteger $ maxIteration) -- Maximum iteration used for while loops as upper-bound
                            cond                             -- Outer loop conditional
                            condInner                        -- Inner loop conditional
                            Empty                            -- Prologue statements (allocate i in the stack)
                            body1                            -- Outer loop body, before inner loop starts
                            flatbody2                        -- Inner loop body
                            flatbody3                        -- Outer loop body, after inner loop returns
        return $ op <> flatloop <> flatep
      (_) -> return top
    (LoopExpansion op (For j _ n body) ep) -> -- Top level (For)
                                              case (pullbackLoop body) of
      (LoopExpansion body1 (For i _ m body2) body3) -> -- Inner loop (For)
                                                       do
        flatbody2 <- loopFlatten' body2
        flatbody3 <- loopFlatten' body3
        flatep    <- loopFlatten' ep
        flatloop  <- mkLoop (n *: m)                         -- Maximum iteration (n*m)
                            (j <: n)                         -- Outer loop conditional (for loop, (j < n))
                            (i <: m)                         -- Inner loop conditional (for loop, (i < m))
                            (j =: 0 <> i =: 0)               -- Prologue statements (allocate i, j in the stack)
                            (body1 <> (i =: 0))              -- Outer loop body, before inner loop starts
                            (flatbody2 <> (i ++:))           -- Inner loop body (for lopp, do i++)
                            (flatbody3 <> (j ++:))           -- Outer loop body, after inner loop returns
        return $ op <> flatloop <> flatep
      (LoopExpansion body1 (While cond body2) body3) -> -- Inner loop (While)
                                                        do
        flatbody2 <- loopFlatten' body2
        flatbody3 <- loopFlatten' body3
        flatep    <- loopFlatten' ep
        flatloop  <- mkLoop (lit . toInteger $ maxIteration) -- Maximum iteration used for while loops as upper-bound
                            (j <: n)                         -- Outer loop conditional (for loop, j < n)
                            cond                             -- Inner loop conditional
                            (j =: 0)                         -- Prologue statements (allocate j in the stack)
                            body1                            -- Outer loop body, before inner loop starts
                            flatbody2                        -- Inner loop body
                            (flatbody3 <> (j ++:))           -- Outer loop body, after inner loop returns
        return $ op <> flatloop <> flatep
      (_) -> return top
    _ -> return top
