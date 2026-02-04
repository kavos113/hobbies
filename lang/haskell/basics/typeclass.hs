class Describable a where
    describe :: a -> String

data Home = House | Apartment deriving (Show, Ord, Eq)

data Dice = S1 | S2 | S3 | S4 | S5 | S6
instance Show Dice where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

instance Eq Dice where 
    (==) S1 S1 = True
    (==) S2 S2 = True
    (==) S3 S3 = True
    (==) S4 S4 = True
    (==) S5 S5 = True
    (==) S6 S6 = True
    (==) _  _  = False

main = do 
    print House
    print S3
    print (S2 == S5) -- False
    print (S4 == S4) -- True
    print (S1 /= S6) -- True
    print (House < Apartment) -- True