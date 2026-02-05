import Data.Semigroup

-- Integer forms a semigroup under addition
instance Semigroup Integer where
  (<>) x y = x + y