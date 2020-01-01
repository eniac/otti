module AST.Simple where
import           Prelude hiding (Num)

{-|

This module presents a simple AST. You can parse it in using whatever parser you want,
or just write it raw for testing

-}

---
--- Types
---

-- | A C type: a signed or unsigned integer of a certain width,
-- a boolean, or a double-precision floating point number
data Type = U8 | S8
          | U16 | S16
          | U32 | S32
          | U64 | S64
          | Bool
          | Double
          | Ptr64 Type
          | Ptr32 Type
          | Struct [Type]
          | Array Int Type
          deriving (Eq, Ord, Show)

numBits :: Type -> Int
numBits U8             = 8
numBits S8             = 8
numBits U16            = 16
numBits S16            = 16
numBits U32            = 32
numBits S32            = 32
numBits U64            = 64
numBits S64            = 64
numBits Bool           = 1
numBits Double         = 64
numBits Ptr64{}        = 64
numBits Ptr32{}        = 32
numBits (Struct tys)   = sum $ map numBits tys
numBits (Array num ty) = num * numBits ty

isSignedInt, isUnsignedInt, isDouble :: Type -> Bool
isSignedInt S8  = True
isSignedInt S16 = True
isSignedInt S32 = True
isSignedInt S64 = True
isSignedInt _   = False
isUnsignedInt U8  = True
isUnsignedInt U16 = True
isUnsignedInt U32 = True
isUnsignedInt U64 = True
isUnsignedInt _   = False
isDouble Double = True
isDouble _      = False
isPointer Ptr64{} = True
isPointer Ptr32{} = True
isPointer _       = False

int8, int16, int32, int64 :: Type -> Bool
int8 S8 = True
int8 U8 = True
int8 _  = False
int16 S16 = True
int16 U16 = True
int16 _   = False
int32 S32 = True
int32 U32 = True
int32 _   = False
int64 S64 = True
int64 U64 = True
int64 _   = False

isStruct :: Type -> Bool
isStruct Struct{} = True
isStruct _        = False

isArray :: Type -> Bool
isArray Array{} = True
isArray _       = False

pointeeType :: Type -> Type
pointeeType (Ptr64 ty) = ty
pointeeType (Ptr32 ty) = ty
pointeeType _          = error "Can't get pointee type of non-pointer"

arrayBaseType :: Type -> Type
arrayBaseType (Array _ ty) = ty
arrayBaseType _            = error "Cannot call arrayBaseType on non-array"

structFieldTypes :: Type -> [Type]
structFieldTypes (Struct tys) = tys
structFieldTypes _ = error "Cannot call structFieldTypes on non-struct"

---
--- Variables
---

-- | Variables have names. Right now, a name is just a string, but we
-- may switch this type out later for better performance (e.g., to word)
type VarName = String

-- | A variable has a name and a type. SSA-ing happens in codegen, *not* in the AST
data Var = Var { varTy   :: Type
               , varName :: VarName
               }
         | StructAccess { baseVar :: Var
                        , field   :: Int
                        }
         | StructPtrAccess { baseVar :: Var
                           , field   :: Int
                           }
         | ArrayAccess { baseVar :: Var
                       , index   :: Expr
                       }
         | ArrayPtrAccess { baseVar :: Var
                          , index   :: Expr
                          }
           deriving (Eq, Ord, Show)

isStructAccess, isStructPtrAccess, isArrayAccess, isArrayPtrAccess, isPtrAccess, isNotPtrAccess, isVar :: Var -> Bool
isStructAccess StructAccess{} = True
isStructAccess _              = False
isStructPtrAccess StructPtrAccess{} = True
isStructPtrAccess _                 = False
isArrayAccess ArrayAccess{} = True
isArrayAccess _             = False
isArrayPtrAccess ArrayPtrAccess{} = True
isArrayPtrAccess _                = False
isPtrAccess ArrayPtrAccess{}  = True
isPtrAccess StructPtrAccess{} = True
isPtrAccess _                 = False
isNotPtrAccess = not . isPtrAccess
isVar Var{} = True
isVar _     = False

-- | Does the variable contain a pointer access?
hasPointerAccess :: Var -> Bool
hasPointerAccess var =
  case var of
    Var{}               -> False
    ArrayPtrAccess{}    -> True
    StructPtrAccess{}   -> True
    ArrayAccess base _  -> hasPointerAccess base
    StructAccess base _ -> hasPointerAccess base

getVarName :: Var -> VarName
getVarName var = case var of
                   Var _ name -> name
                   _          -> varName $ baseVar var

---
--- Numbers
---

-- | I'm seperating out the different types of numbers here, especially because
-- proof system code will want a variety of interesting number types (or, for that matter,
-- crypto code for symexing). Representing all these numbers with a single Haskell type
-- is not realistic, so we wrap the number type in an ADT
data Num = INum { numTy  :: Type
                , numVal :: Integer
                }
         | FNum { numTy    :: Type
                , floatVal :: Double
                }
         deriving (Eq, Ord, Show)

---
--- AST definition
---

-- | An AST expression: link
data Expr = VarExpr { varExpr :: Var }
          | NumExpr { numExpr :: Num }
          | Neg Expr
          | Not Expr
          | Abs Expr
          | Eq Expr Expr
          | NEq Expr Expr
          | And Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Or Expr Expr
          | XOr Expr Expr
          | Min Expr Expr
          | Max Expr Expr
          | Gt Expr Expr
          | Gte Expr Expr
          | Lt Expr Expr
          | Lte Expr Expr
          | Shl Expr Expr
          | Shr Expr Expr
          | Tern Expr Expr Expr
          | Cast Expr Type
          | Call FunctionName [Expr]
          | Load Expr
            deriving (Eq, Ord, Show)

-- | An AST statement: link
data Stmt = Decl Var
          | Assign Var Expr
          | Store Var Expr
          | If Expr [Stmt] [Stmt]
          | While Expr [Stmt]
          | VoidCall FunctionName [Expr]
          | Return Expr
          | VoidReturn

---
--- Functions and programs
---

type FunctionName = String

data Function = Function { fName :: FunctionName
                         , fTy   :: Type
                         , fArgs :: [(VarName, Type)]
                         , fBody :: [Stmt]
                         }

-- | A program is function definitions and class definitions
data Program = Program { functions :: [Function] }


