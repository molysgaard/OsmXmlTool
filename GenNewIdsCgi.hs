{--import qualified Data.Map as Map
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
--}

-- Accepts file uploads and saves the files in the given directory.
-- WARNING: this script is a SECURITY RISK and only for 
-- demo purposes. Do not put it on a public web server.
import qualified Data.Map as Map
import Control.Monad.State (runState)
import System (getArgs)

import Text.XML.HaXml (render)
import Text.XML.HaXml.Pretty

import GenNew
import GenXml
import ParseXml
 
import Network.CGI
import Text.XHtml
 
import qualified Data.ByteString.Lazy as BS
 
import Control.Monad (liftM)
import Data.Maybe (fromJust)
 
fileForm = form ! [method "post", enctype "multipart/form-data"]
             << [afile "file", submit "" "Upload"]

saveFile :: (MonadCGI m) => m (Maybe String)
saveFile = do
       let converter = render . document . genXml . fst . (\f -> runState (genNewIds f) (0,Map.empty)) . parseXml
       cont <- liftM (liftM converter) $ getInput "file"
       return $ cont
 
page t b = header << thetitle << t +++ body << b
 
basename = reverse . takeWhile (`notElem` "/\\") . reverse
 
cgiMain = 
    do ret <- saveFile
       case ret of
         Nothing -> do
                    output . renderHtml $ page "Upload example" fileForm
         Just h -> do
                    setHeader "Content-type" "text/xml"
                    output h
 
main = runCGI $ handleErrors cgiMain

