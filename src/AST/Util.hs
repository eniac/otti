module AST.Util where

-- An annotated AST
data Annotated a s = Annotated { ast :: !a
                               , ann :: !s
                               } deriving (Eq)
instance Show a => Show (Annotated a s) where
  show = show . ast

mapAnns :: Functor a => (s -> t) -> Annotated (a s) s -> Annotated (a t) t
mapAnns f (Annotated x y) = Annotated (f <$> x) (f y)

mapAnnsL
  :: Functor a
  => (s -> t)
  -> Annotated [Annotated (a s) s] s
  -> Annotated [Annotated (a t) t] t
mapAnnsL f (Annotated x y) = Annotated (map (mapAnns f) x) (f y)

instance Functor (Annotated a) where
  fmap f (Annotated x y) = Annotated x (f y)

class Interval b where
  (\/) :: b -> b -> b
  union :: b -> [b] -> b
  union = foldr (\/)

-- Line, Column
data Posn = Posn !Int !Int !Int deriving (Eq,Ord)
data PosnPair = PosnPair !Posn !Posn deriving (Show,Eq)
data Span = Span FilePath !Posn !Posn deriving (Show,Eq)

nullPosn :: Posn
nullPosn = Posn 0 0 0

nullSpan :: Span
nullSpan = Span "<unknown>" (Posn 0 0 0) (Posn 0 0 0)


instance Show Posn where
  show (Posn _ l c) = show l ++ ":" ++ show c

instance Interval PosnPair where
  (PosnPair a b) \/ (PosnPair c d) = PosnPair (min a c) (max b d)

instance Interval Span where
  (Span p1 a b) \/ (Span p2 c d) = if p1 == p2
    then Span p1 (min a c) (max b d)
    else
      error
      $  "Paths "
      ++ p1
      ++ " and "
      ++ p2
      ++ " differ. Cannot unify spans over them!"
