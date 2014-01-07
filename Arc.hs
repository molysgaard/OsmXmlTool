module Arc where

floatMod a b = let whole = fromIntegral $ truncate (a/b)
               in a - (whole * b)

data PRad = PRad Double Double
data LatLon = LatLon Double Double deriving (Eq, Show, Read)
pRadToLat (PRad a b) = LatLon (a/pi*180) (b/pi*180)
latToPRad (LatLon a b) = PRad (a/180*pi) (b/180*pi)

data Deg = Deg Double
data Rad = Rad Double
degToRad (Deg a) = Rad (-a/180*pi)
radToDeg (Rad a) = Deg (-a/pi*180)

smallCirclePoint :: PRad -> Double -> Rad -> PRad
smallCirclePoint (PRad lat1 lon1) d (Rad tc) = let
     lat = asin (sin lat1 *cos d +cos lat1 *sin d *cos tc)
     dlon = atan2 (sin tc *sin d *cos lat1) (cos d -sin lat1 *sin lat)
     lon = (floatMod (lon1-dlon+pi) (2*pi)) - pi
     in (PRad lat lon)


earthRadius = 6384.0e3

circularDiff a b m dir
  | dir = floatMod (b-a) m
  | otherwise = floatMod (a-b) m

genCircularRange :: Double -> Double -> Double -> Bool -> Int -> [Double]
genCircularRange a b m dir fullNum =
    let diff = circularDiff a b m dir
        num = ceiling (diff/m*(fromIntegral fullNum))
    in if dir
         then let step = diff/(fromIntegral num)
              in fun num step
         else let step = -diff/(fromIntegral num)
              in fun num step
    where fun num step = map (flip floatMod m) (map (\x -> a+step*x) [0,1..(fromIntegral num)])

genArc :: LatLon -> Double -> Deg -> Deg -> Bool -> Int -> [LatLon]
genArc ll d (Deg tc1) (Deg tc2) dir num =
    let range = genCircularRange tc1 tc2 360 dir num
        tcs = map (degToRad . Deg) range
    in map (pRadToLat . (smallCirclePoint (latToPRad ll) (d/earthRadius))) tcs

courceBetweenPoints :: LatLon -> LatLon -> Deg
courceBetweenPoints ll1 ll2 =
    let (PRad lat1 lon1) = latToPRad ll1
        (PRad lat2 lon2) = latToPRad ll2
    in radToDeg . Rad $ floatMod (atan2 (sin (lon1-lon2) *cos lat2) (cos lat1 *sin lat2 -sin lat1 *cos lat2 *cos (lon1-lon2))) (2*pi)

distanceBetweenPoints :: LatLon -> LatLon -> Double
distanceBetweenPoints ll1 ll2 =
    let (PRad lat1 lon1) = latToPRad ll1
        (PRad lat2 lon2) = latToPRad ll2
    in 2*earthRadius*asin (sqrt ((sin ((lat1-lat2)/2))^2 + 
         cos lat1 *cos lat2 * (sin ((lon1-lon2)/2))^2))
