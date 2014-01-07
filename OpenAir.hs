{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Prelude hiding (putStrLn, readFile)
import Control.Monad
import Data.Char (toLower)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.IO
import Text.Parsec
import Text.Parsec.Text.Lazy
import Text.Parsec.Perm
import qualified Text.Parsec.Token as Token
import Arc
import System.Environment

import Data.Maybe

data St = St { x :: LatLon
             , dir :: Bool
             }

type PText = T.Text

type OpenAir st a = GenParser st a

languageDef =
  Token.LanguageDef { Token.commentLine     = "*"
                    , Token.commentStart    = "*"
                    , Token.commentEnd      = "\n"
                    , Token.identStart      = letter
                    , Token.identLetter     = alphaNum
                    , Token.reservedNames   = [ "AR"
                                              , "AN"
                                              , "AH"
                                              , "AL"
                                              , "AT"
                                              , "TO"
                                              , "TC"
                                              , "SP"
                                              , "SB"
                                              , "V"
                                              , "DP"
                                              , "DB"
                                              , "DC"
                                              , "DY"
                                              ]
                    , Token.caseSensitive   = False
                    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
--integer    = Token.integer    lexer -- parses an integer
--float      = Token.float      lexer -- parses a float
colon      = Token.colon      lexer -- parse a colon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
comma      = Token.comma      lexer
naturalOrFloat = Token.naturalOrFloat lexer

colonSep p = sepBy p colon
colonSep1 p = sepBy1 p colon

number :: OpenAir St Double
number = do
    n <- naturalOrFloat
    case n of
        Left a -> return $ fromIntegral a
        Right a -> return a

degminsec' :: OpenAir St (Double, Double, Double)
degminsec' = do
    ls <- try (colonSep number)
    whiteSpace
    if length ls == 3
        then return (head ls, ls !! 1, ls !! 2)
        else fail "Unparsable deg:min:sec triple."

degminsec :: OpenAir St Double
degminsec = do
    (a,b,c) <- degminsec'
    dir <- liftM toLower $ oneOf "NnSsEeWw"
    whiteSpace
    let t = a + b/60 + c/60^2
    case dir of
        'n' -> return t
        'e' -> return t
        's' -> return (-t)
        'w' -> return (-t)

pLatLon :: OpenAir St LatLon
pLatLon = do
    lat <- degminsec
    lon <- degminsec
    return (LatLon lat lon)

pClass :: OpenAir St PText
pClass = do
    reserved "AC"
    liftM T.pack identifier

untillEOL :: OpenAir St PText
untillEOL = do
  liftM T.pack $ manyTill anyToken newline

pName :: OpenAir St PText
pName = do
    reserved "AN"
    untillEOL

parseLengthUnit :: OpenAir St LengthUnit
parseLengthUnit =
  let ft = string "FT" >> return FT
      m  = string "M" >> return M
      fallback = return FT
  in try ft <|> m <|> fallback

pHeight :: OpenAir St Height
pHeight = do
    let gnd = do
           choice [string "GND", string "MSL"]
           return GND
        unl = string "UNL" >> return UNL
        fl = string "FL" >> number >>= return . FL
        agl = do
            num <- number
            choice [string "AGL", string "GND"]
            unit <- parseLengthUnit
            if num==0
                then return GND
                else return (AGL num unit)
        amsl = do
            num <- number
            choice [string "AMSL", string "MSL"]
            unit <- parseLengthUnit
            return (AMSL num unit)
    ans <- try amsl <|> try agl <|> try fl <|> try gnd <|> unl
    whiteSpace
    return ans

pHeightUpper :: OpenAir St Height
pHeightUpper = do
    reserved "AH"
    pHeight

pHeightLower :: OpenAir St Height
pHeightLower = do
    reserved "AL"
    pHeight

pLabel :: OpenAir St LatLon
pLabel = do
    reserved "AT"
    pLatLon

pTerrainOpenName :: OpenAir St PText
pTerrainOpenName = do
    reserved "TO"
    untillEOL

pTerrainClosedName :: OpenAir St PText
pTerrainClosedName = do
    reserved "TC"
    untillEOL

pPen = do
    reserved "SP"
    untillEOL
    return Nothing

pColor = do
    reserved "SB"
    untillEOL
    return Nothing

pSet = try pSet1 <|> pSet2

pSet1 = do
    reserved "V"
    var <- identifier
    char '='
    whiteSpace
    case var of
        "D" -> setDir
        "X" -> setCenter
        "W" -> setAwyWidth
        "Z" -> setZoom

pSet2 = do
    reserved "V"
    char '='
    whiteSpace
    var <- identifier
    case var of
        "D" -> setDir
        "X" -> setCenter
        "W" -> setAwyWidth
        "Z" -> setZoom

setDir = do
    c <- oneOf "+-"
    st <- getState
    case c of
      '-' -> setState (st {dir = False}) >> whiteSpace >> return Nothing
      '+' -> setState (st {dir = True}) >> whiteSpace >> return Nothing
setCenter = do
    st <- getState
    ll <- pLatLon
    setState (st {x = ll}) >> whiteSpace >> return Nothing
setAwyWidth = untillEOL >> whiteSpace >> return Nothing
setZoom = untillEOL >> whiteSpace >> return Nothing

pPoint = do
    reserved "DP"
    liftM (Just . (:[])) pLatLon

pArcSector = do
    reserved "DA"
    r <- number
    comma
    startAngle <- number
    comma
    endAngle <- number
    st <- getState
    return $ Just $ genArc (x st) (1.852e3*r) (Deg startAngle) (Deg endAngle) (dir st) 25

pArcTwoPoints = do
    reserved "DB"
    ll1 <- pLatLon
    comma
    ll2 <- pLatLon
    st <- getState
    let (Deg tc1) = courceBetweenPoints (x st) ll1
        (Deg tc2) = courceBetweenPoints (x st) ll2
        r = distanceBetweenPoints (x st) ll1
    return . Just $ genArc (x st) r (Deg tc1) (Deg tc2) (dir st) 25

pCircle = do
    reserved "DC"
    r <- number
    st <- getState
    return . Just $ genArc (x st) (1.852e3*r) (Deg 0) (Deg 346) True 25

pAirway = do
    reserved "DY"
    untillEOL
    return Nothing

fixthings :: [Maybe a] -> Maybe [a]
fixthings xs =
    let ys = catMaybes xs
    in if length ys==0
        then Nothing
        else Just ys

pAirspace = do
    cls <- pClass
    let header = permute ((,,) <$$> (choice $ map try [pName, pTerrainOpenName, pTerrainClosedName])
                               <||> try pHeightLower
                               <||> try pHeightUpper)
    (aName, lHeight, uHeight) <- header
    let segment = map try [pSet, pArcTwoPoints, pArcSector, pPoint, pAirway, pPen, pColor]
        --circ = map try [pSet, pCircle]
        j = choice [pSet >> pCircle, (liftM . liftM) concat $ liftM fixthings $ many1 (choice segment)]
    segs <- j
    return Airspace { aName = aName
                    , aClass = cls
                    , aLowerHeight = lHeight
                    --, aLowerRef = lRef
                    , aUpperHeight = uHeight
                    --, aUpperRef = uRef
                    , aSegments = fromJust segs
                    }
    --return (aName, (lNum, lRef), (uNum, uRef), segs)

--data HeightRef = GND | AGL | AMSL | FL deriving (Eq, Read, Show)

data LengthUnit = FT | M deriving (Eq, Read, Show)
data Height = GND | UNL | FL Double | AGL Double LengthUnit | AMSL Double LengthUnit deriving (Eq, Show, Read)

data Airspace = Airspace {
                      aName :: PText
                    , aClass :: PText
                    , aLowerHeight :: Height
                    --, aLowerRef :: HeightRef
                    , aUpperHeight :: Height
                    --, aUpperRef :: HeightRef
                    , aSegments :: [LatLon]
                    } deriving (Eq, Show, Read)

testll = "12:12:12 S 189:1:3 E"

testAs = unlines ["AC R",
                  "AN ED-R37B NORDHORN",
                  "AL GND",
                  "AH 2500 MSL",
                  "DP 52:43:05 N 007:03:49 E",
                  "DP 52:41:00 N 007:15:10 E",
                  "DP 52:29:31 N 007:17:48 E",
                  "V D=-",
                  "V X=52:26:00 N 007:12:00 E",
                  "DB 52:29:31 N 007:17:48 E,52:26:00 N 007:03:50 E",
                  "DP 52:26:00 N 007:03:39 E",
                  "DP 52:39:59 N 007:03:27 E",
                  "AC R",
                  "AN GRESSS!!!",
                  "AL GND",
                  "AH 2500MSL",
                  "DP 52:43:05 N 007:03:49 E",
                  "DP 52:41:00 N 007:15:10 E",
                  "DP 52:29:31 N 007:17:48 E",
                  "V D= -",
                  "V X=52:26:00 N 007:12:00 E",
                  "DB 52:29:31 N 007:17:48 E,52:26:00 N 007:03:50 E",
                  "DP 52:26:00 N 007:03:39 E",
                  "DP 52:39:59 N 007:03:27 E"]

main :: IO ()
main = do
    args <- getArgs
    let st = St {x = undefined, dir = undefined}
    f <- readFile (head args)
    case runP (whiteSpace >> manyTill pAirspace eof) st (head args) f of
        Left err -> putStrLn . T.pack . show $ err
        --Right z -> putStrLn . T.pack . show $ (z, length z, length $ filter (null . aSegments) z)
        Right (z :: [Airspace]) -> putStrLn . (T.intercalate "\n") $ (map (\y -> T.intercalate "\n" [aName y, T.pack . show $ aLowerHeight y]) z :: [T.Text])
