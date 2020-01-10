{-# LANGUAGE MultiParamTypeClasses #-}
module IR.WASM where
import           AST.WASM
import           IR.IR
import           Targets.SMT (Node)
import qualified Targets.SMT as SMT

data WASMNode = WASMNode { wasmNode :: Node
                         , wasmType :: ValueType
                         }

instance IRNode WASMNode ValueType where
  n = wasmNode
  t = wasmType
