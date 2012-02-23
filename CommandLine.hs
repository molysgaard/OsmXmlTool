import qualified Data.Map as Map
import Control.Monad.State (runState)
import System (getArgs)

import Text.XML.HaXml
import Text.XML.HaXml.Pretty

import GenNew
import GenXml
import ParseXml

main :: IO ()
main = do --interact (render . document . genXml . fst . (\f -> runState (genNewIds f) (0,Map.empty)) . parseXml)
    args <- getArgs
    input <- readFile (args !! 0)
    let out = render . document . genXml . fst . (\f -> runState (genNewIds f) (0,Map.empty)) $ parseXml input
    writeFile (args !! 1) out
