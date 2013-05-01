import Text.ParserCombinators.Parsec hiding (newline)
import Text.ParserCombinators.Parsec.Number
import Arc
import GenNew hiding (St)
import qualified Data.Map as Map
import Control.Monad (liftM)

-- for testing
import Text.XML.HaXml hiding (x)
import Text.XML.HaXml.Pretty
import GenXml
import Control.Monad.State (runState)
import System.Environment

newline = try (string "\r\n") <|> try (string "\n")

data St = St { x :: LatLon
             , curId :: Integer
             , file :: File
             , dir :: Bool
             }
type OpenAir a = GenParser Char St a 

parseFloat :: (Num a, Floating a) => OpenAir a
parseFloat = try floating <|> (liftM fromIntegral decimal)

parseNums :: OpenAir [Double]
parseNums = sepBy1 parseFloat (char ':')

parseLat = do
    nums <- parseNums
    optional space
    ch <- oneOf "NSns"
    let decimal = (nums !! 0) + (nums !! 1)/60 + (nums !! 2)/60^2
    if ch == 'N' || ch == 'n'
      then return decimal
      else return (-decimal)

parseLon = do
    nums <- parseNums
    optional space
    ch <- oneOf "EWew"
    let decimal = (nums !! 0) + (nums !! 1)/60 + (nums !! 2)/60^2
    if ch == 'E' || ch == 'e'
      then return decimal
      else return (-decimal)

parseLatLon :: OpenAir LatLon
parseLatLon = do
    lat <- parseLat
    space
    lon <- parseLon
    return (LatLon lat lon)

parseLenthUnit = try (string "FT")

parseHeight :: (Num a, Floating a) => OpenAir (a, String)
parseHeight = do
    let gnd = ((string "GND" <|> string "MSL") >> return (0,"AGL"))
        unl = (string "UNL" >> return (999, "FL"))
        fl = do
              string "FL"
              num <- parseFloat
              return (num, "FL")
        r = do
              num <- parseFloat
              unit <- optional parseLenthUnit
              space
              ref <- (try (string "MSL" >> return "AMSL") <|> try (string "AGL") <|> (string "GND" >> return "AGL"))
              return (num, ref)
    ans <- try r <|> try fl <|> try gnd <|> unl
    return ans

parseLowerHeight = do
    string "AL "
    h <- parseHeight
    newline
    return h

parseUpperHeight = do
    string "AH "
    h <- parseHeight
    newline
    return h

strings :: [String] -> OpenAir String
strings (s:ss) = foldl (\st v -> st <|> try (string v)) (try (string s)) ss

parseClass :: OpenAir String
parseClass = do
    string "AC "
    c <- strings ["CTR","TMZ","GP","R","Q","P","A","B","C","D","E","F","G","W"]
    newline
    return c

parseName :: OpenAir String
parseName = try (string "AN " >> manyTill anyChar newline) <|> try (string "TO " >> manyTill anyChar newline) <|> try (manyTill anyChar newline)

parseSet :: OpenAir [LatLon]
parseSet = do
    string "V "
    (try parseX) <|> parseDir
    return []

parseDir = try parseDirV1 <|> try parseDirV2

parseDirV1 = do
    char 'D'
    optional space
    char '='
    optional space
    c <- oneOf "+-"
    newline
    st <- getState
    case c of
      '-' -> setState (st {dir = False})
      '+' -> setState (st {dir = True})

parseDirV2 = do
    char '='
    optional space
    char 'D'
    optional space
    c <- oneOf "+-"
    newline
    st <- getState
    case c of
      '-' -> setState (st {dir = False})
      '+' -> setState (st {dir = True})

parseX :: OpenAir ()
parseX = do
    char 'X'
    optional space
    char '='
    optional space
    st <- getState
    ll <- parseLatLon
    newline
    setState (st {x = ll})

newId :: OpenAir Integer
newId = do
    st <- getState
    let n = curId st
    setState (st {curId = (curId st +1)})
    return n

toNode :: LatLon -> OpenAir Node
toNode (LatLon lat lon) = do
    i <- newId
    return $ Node {nId=i, lat=lat, lon=lon, nTags=Map.empty}

addNodeToFile :: Node -> OpenAir ()
addNodeToFile n = do
    st <- getState
    let (File ns ws rs) = file st
    setState (st {file =File (n:ns) ws rs})

addWayToFile :: Way -> OpenAir ()
addWayToFile w = do
    st <- getState
    let (File ns ws rs) = file st
    setState (st {file =File ns (w:ws) rs})

createWay lls tags = do
    ns <- mapM toNode lls
    let membs = map nId ns
    wid <- newId
    let w = Way {wId = wid, wMembs = membs, wTags=tags}
    mapM_ addNodeToFile ns
    addWayToFile w

createArea lls tags = do
    ns <- mapM toNode lls
    let membs@(m:_) = map nId ns
    wid <- newId
    let w = Way {wId = wid, wMembs = (membs++[m]), wTags=tags}
    mapM_ addNodeToFile ns
    addWayToFile w

parseCircle :: OpenAir [[LatLon]]
parseCircle = do
    string "DC "
    d <- parseFloat
    newline
    st <- getState
    return $ [genArc (x st) (1.852e3*d) (Deg 0) (Deg 346) True 25]

parseArc1 :: OpenAir [LatLon]
parseArc1 = do
    string "DA "
    r <- parseFloat
    char ','
    tc1 <- parseFloat
    char ','
    tc2 <- parseFloat
    newline
    st <- getState
    return $ genArc (x st) (1.852e3*r) (Deg tc1) (Deg tc2) (dir st) 25

parseArc2 :: OpenAir [LatLon]
parseArc2 = do
    string "DB "
    ll1 <- parseLatLon
    char ','
    optional space
    ll2 <- parseLatLon
    newline
    st <- getState
    let (Deg tc1) = courceBetweenPoints (x st) ll1
        (Deg tc2) = courceBetweenPoints (x st) ll2
        r = distanceBetweenPoints (x st) ll1
    return $ genArc (x st) r (Deg tc1) (Deg tc2) (dir st) 25

parseCoord :: OpenAir [LatLon]
parseCoord = do
    string "DP "
    ll <- parseLatLon
    optional $ many space
    try commentLine <|> try (newline >> return '')
    return . return $ ll

parseAt :: OpenAir ()
parseAt = do
    string "AT "
    parseLatLon
    return ()

parseSegment :: OpenAir [[LatLon]]
parseSegment = do
    let arc1 = parseArc1
        arc2 = parseArc2
        coord = parseCoord
        circle = parseCircle
        set = parseSet
        cm = try (commentLine >> return [])
    optional cm
    many (try set)
    optional cm
    (try circle) <|> (many1 (cm <|> try set <|> try arc1 <|> try arc2 <|> coord))

parseAirspace :: OpenAir ()
parseAirspace = do
    st <- getState
    --setState (st {dir = True})
    let cm = optional commentLine
    aClass <- parseClass
    try cm
    aName <- parseName -- parseName parses a newline
    try cm
    (lNum,lRef) <- parseLowerHeight
    try cm
    (uNum,uRef) <- parseUpperHeight
    try cm
    lls <- parseSegment
    let tags = Map.fromList [("airspace","yes")
                            ,("name",aName)
                            ,("airspace:class","ft")
                            ,("height:lower",show lNum)
                            ,("height:lower:ref",lRef)
                            ,("height:lower:unit","ft")
                            ,("height:upper",show uNum)
                            ,("height:upper:ref",uRef)
                            ,("height:upper:unit","ft")]
    createArea (concat lls) tags

commentLine = do
    char '*'
    manyTill anyChar newline
    return ()

emptyLine = newline >> return ()


parseFile = do
    manyTill (try emptyLine <|> try commentLine <|> try parseAirspace) eof
    st <- getState
    return (file st)
    

--testAs1 = unlines ["AC R",
--                  "AN ED-R37B NORDHORN",
--                  "AL GND",
--                  "AH 2500MSL",
--                  "V X=52:26:00 N 007:12:00 E",
--                  "DB 52:29:31 N 007:17:48 E,52:26:00 N 007:03:50 E"]
--
--testAs = unlines ["AC R",
--                  "AN ED-R37B NORDHORN",
--                  "AL GND",
--                  "AH 2500MSL",
--                  "DP 52:43:05 N 007:03:49 E",
--                  "DP 52:41:00 N 007:15:10 E",
--                  "DP 52:29:31 N 007:17:48 E",
--                  "V D=-",
--                  "V X=52:26:00 N 007:12:00 E",
--                  "DB 52:29:31 N 007:17:48 E,52:26:00 N 007:03:50 E",
--                  "DP 52:26:00 N 007:03:39 E",
--                  "DP 52:39:59 N 007:03:27 E",
--                  "AC R",
--                  "AN GRESSS!!!",
--                  "AL GND",
--                  "AH 2500MSL",
--                  "DP 52:43:05 N 007:03:49 E",
--                  "DP 52:41:00 N 007:15:10 E",
--                  "DP 52:29:31 N 007:17:48 E",
--                  "V D=-",
--                  "V X=52:26:00 N 007:12:00 E",
--                  "DB 52:29:31 N 007:17:48 E,52:26:00 N 007:03:50 E",
--                  "DP 52:26:00 N 007:03:39 E",
--                  "DP 52:39:59 N 007:03:27 E"]
--
--test = do
--    let st = St { x = LatLon 0 0
--                , file = File [] [] []
--                , curId = 0
--                , dir = True}
--    let ret = runParser parseFile st "test" testAs
--    case ret of
--      Right file -> do
--        let f = runState (genNewIds file) (0,Map.empty)
--        let out = render . document . genXml . fst $ f
--        putStrLn out
--      Left err -> putStrLn . show $ err
--
test f t = do
    let st = St { x = LatLon 0 0
                , file = File [] [] []
                , curId = 0
                , dir = True}
    runParser f st "test" t

main = do
    let st = St { x = LatLon 0 0
                , file = File [] [] []
                , curId = 0
                , dir = True}
    args <- getArgs
    putStrLn . show $ args
    input <- readFile (args !! 0)
    let ret = runParser parseFile st (args !! 0) input
    case ret of
      Right file -> do
        let f = runState (genNewIds file) (0,Map.empty)
        let out = render . document . genXml . fst $ f
        writeFile (args !! 1) out
      Left err -> putStrLn . show $ err
