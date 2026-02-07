import Data.Map qualified as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB =
  Map.fromList
    [ ("Tokyo", (35.6895, 139.6917)),
      ("New York", (40.7128, -74.0060)),
      ("London", (51.5074, -0.1278)),
      ("Sydney", (-33.8688, 151.2093))
    ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRadians :: LatLong -> (Double, Double)
latLongToRadians (lat, long) = (toRadians lat, toRadians long)

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = radius * c
  where
    (lat1, lon1) = latLongToRadians coords1
    (lat2, lon2) = latLongToRadians coords2
    dlat = lat2 - lat1
    dlon = lon2 - lon1
    a =
      sin (dlat / 2) ^ 2
        + cos lat1 * cos lat2 * sin (dlon / 2) ^ 2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    radius = 6371.0 -- Earth's radius in kilometers

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "One of the locations is not in the database."
printDistance (Just distance) = putStrLn ("The distance is " ++ show distance ++ " km.")

main :: IO ()
main = do
  putStrLn "Enter the first location:"
  loc1 <- getLine
  let city1 = Map.lookup loc1 locationDB

  putStrLn "Enter the second location:"
  loc2 <- getLine
  let city2 = Map.lookup loc2 locationDB

  let distance = haversine <$> city1 <*> city2
  printDistance distance
