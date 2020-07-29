module AST.Cranelift where
import           AST.Typed

-- | https://cranelift.readthedocs.io/en/latest/ir.html#value-types
data CraneliftTy = B1 | B8 | B16 | B32 | B64
                 | I8 | I16 | I32 | I64
                 | F32 | F64
                 deriving (Eq, Ord, Show)


instance Typed CraneliftTy where
  numBits B1  = 1
  numBits B8  = 8
  numBits B16 = 16
  numBits B32 = 32
  numBits B64 = 64
  numBits I8  = 8
  numBits I16 = 16
  numBits I32 = 32
  numBits I64 = 64
  numBits F32 = 32
  numBits F64 = 64

  isSignedInt _ = False
  isUnsignedInt I8  = True
  isUnsignedInt I16 = True
  isUnsignedInt I32 = True
  isUnsignedInt I64 = True
  isUnsignedInt _   = False

  isDouble F32 = True
  isDouble F64 = True

  isPointer I32 = True
  isPointer I64 = True
  isPointer _   = False

  -- Not relevant
  pointeeType = error "Not usable"
  isStruct = error "Not usable"
  isArray = error "Not usable"
  structFieldTypes = error "Not usable"
  arrayBaseType _ = error "Not usable"
  arrayNumElems _ = error "Not usable"
  newStructType _ = error "Not usable"
  newArrayType _ _ = error "Not usable"
  structFieldList _ = error "Not usable"

data Var = Var { varName :: String
               , varTy   :: CraneliftTy
               }
         deriving (Eq, Ord, Show)

data Const = IConst Int CraneliftTy
           | FConst Double CraneliftTy
           | BConst Bool CraneliftTy
           deriving (Eq, Ord, Show)

data Op = ConstOp Const | VarOp Var

data CmpOp = Blah

data Expr = Load CraneliftTy Op
          | StackLoad CraneliftTy Op
          | Icmp CmpOp Op Op
          | Fadd Op Op

data BB = BB String [Var]
data JumpTable = JumpTable

data Stmt = Assign Var Expr
          | Store Op Var
          | StackStore Op Var
          | Return Op
          | Jump BB
          | Brz Op BB
          | Brnz Op BB
          | BrICmp Expr Op Op BB
          | BrTable Op BB JumpTable
          | EBB [(Var, CraneliftTy)] [Stmt]

