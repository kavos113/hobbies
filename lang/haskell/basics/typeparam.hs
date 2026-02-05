data Box a = Box a deriving (Show)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving (Show)

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ y _) = y

third :: Triple a -> a
third (Triple _ _ z) = z

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

type Point3D = Triple Double

origin :: Point3D
origin = Triple 0.0 0.0 0.0

somePoint :: Point3D
somePoint = Triple 1.0 2.0 3.0

main = do
  print origin
  print (wrap 42)
  print (unwrap (Box "Hello"))
  print (toList somePoint)
  print (transform (+ 1) somePoint)