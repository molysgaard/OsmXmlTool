import qualified Data.Map as Map
import Control.Monad.State (runState)
import System (getArgs)

import Text.XML.HaXml
import Text.XML.HaXml.Pretty

import GenNew
import GenXml
import ParseXml

noTags = Map.empty
n1 = Node {nId = 1, nVer=0, lat = 0, lon = 0, nTags = Map.fromList [("name","alpha")]}
n2 = Node {nId = 2, nVer=0, lat = 1, lon = 0, nTags = Map.fromList [("name","bravo")]}
n3 = Node {nId = 3, nVer=0, lat = 0, lon = 1, nTags = Map.fromList [("name","charlie")]}
w1 = Way  {wId = 4, wVer=0, wMembs = [1,2,3], wTags = Map.fromList [("name","delta")]}
r1 = Relation {rId = 5, rVer=0, rMembs = [(1,""),(3,""),(4,"")], rTags = Map.fromList [("name","echo")]}

file = File [n1,n2,n3] [w1] [r1]

test = runState (genNewIds file) (0,Map.empty)

main :: IO ()
main = do --interact (render . document . genXml . fst . (\f -> runState (genNewIds f) (0,Map.empty)) . parseXml)
    args <- getArgs
    input <- readFile (args !! 0)
    let out = render . document . genXml . fst . (\f -> runState (genNewIds f) (0,Map.empty)) $ parseXml input
    writeFile (args !! 1) out
