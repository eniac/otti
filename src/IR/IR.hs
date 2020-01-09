module IR.IR where
import           IR.SMTIRMonad
import           Targets.SMT   (Node)
import qualified Targets.SMT   as SMT

-- | IR node.
-- An IR node consists of an SMT node and some optional other fields
data IRNode a = IRNode { smtNode    :: Node
                       , extraState :: a
                       } deriving (Eq, Ord, Show)

type PlainNode = IRNode ()

mkPlainNode :: Node -> PlainNode
mkPlainNode n = IRNode n ()

irInt :: Int -- ^ Width
      -> Bool -- ^ Signedness
      -> Integer -- ^ Value
      -> IR PlainNode -- ^ Resulting int node
irInt width signed val = liftSMT $ do
  int <- case width of
           1  | val <= 1 -> SMT.bvNum 1 val
           1  -> error $ unwords $ [show val, "is past the range of a boolean"]
           8  | not signed && val <= 255 -> SMT.bvNum 8 val
           8  | not signed -> error $ unwords $ [show val, "is past the range of an i8"]
           8  | signed && val <= 127 -> SMT.bvNum 8 val
           8  | signed -> error $ unwords $ [show val, "is past the range of a signed i8"]
           16 | not signed && val <= 65535 -> SMT.bvNum 16 val
           16 | not signed -> error $ unwords $ [show val, "is past the range of an i16"]
           16 | signed && val <= 32767 -> SMT.bvNum 16 val
           16 | signed -> error $ unwords $ [show val, "is past the range of a signed i16"]
           32 | not signed && val <= 4294967295 -> SMT.bvNum 32 val
           32 | not signed -> error $ unwords $ [show val, "is past the range of an i32"]
           32 | signed && val <= 2147483647 -> SMT.bvNum 32 val
           32 | signed -> error $ unwords $ [show val, "is past the range of a signed i32"]
           64 | not signed && val <= 18446744073709551615 -> SMT.bvNum 64 val
           64 | not signed -> error $ unwords $ [show val, "is past the range of an i64"]
           64 | signed && val <= 9223372036854775807 -> SMT.bvNum 64 val
           64 | signed -> error $ unwords $ [show val, "is past the range of a signed i64"]
           _ -> SMT.bvNum width val
  return $ mkPlainNode int



