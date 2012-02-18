module GenNew where

import Data.Map as Map
import Control.Monad (liftM)
import Control.Monad.State
import Data.Maybe (catMaybes)

type Tags = Map.Map String String

data Node = Node { nId :: Integer
                 --, nVer :: Integer
                 , lat :: Double
                 , lon :: Double
                 , nTags :: Tags } deriving (Eq, Show)
data Way = Way { wId :: Integer
               --, wVer :: Integer
               , wMembs :: [Integer]
               , wTags :: Tags } deriving (Eq, Show)
data Relation = Relation { rId :: Integer
                         --, rVer :: Integer
                         , rMembs :: [(Integer,String)]
                         , rTags :: Tags } deriving (Eq, Show)

data File = File [Node] [Way] [Relation] deriving (Show)
type St = State (Integer, Map.Map Integer (Integer,String))

genNewIds :: File -> St File
genNewIds (File ns ws rs) = do
    n <- mapM newNode ns
    w <- mapM newWay ws
    r <- mapM newRel rs
    return $ File n w r

addOsmId i m = Map.insert "OSM-ID" (show i) m
addOsmVer v m = Map.insert "OSM-VER" (show v) m
--addTags i v m = addOsmVer v $ addOsmId i m
addTags i v m = addOsmId i m

newNode :: Node -> St Node
newNode n = do i <- getNext (nId n) "node"
               return $ n {nId = i, nTags = addTags (nId n) 0 (nTags n)}

newWay :: Way -> St Way
newWay n = do i <- getNext (wId n) "way"
              membs <- liftM catMaybes $ mapM newWayMem (wMembs n)
              return $ n {wId = i, wMembs = membs, wTags = addTags (wId n) 0 (wTags n)}

newRel :: Relation -> St Relation
newRel n = do i <- getNext (rId n) "relation"
              membs <- liftM catMaybes $ mapM newRelMem (rMembs n)
              return $ n {rId = i, rMembs = membs, rTags = addTags (rId n) 0 (rTags n)}

newWayMem :: Integer -> St (Maybe Integer)
newWayMem i = do (_,kv) <- get
                 return . (liftM fst) $ Map.lookup i kv

newRelMem :: (Integer,String) -> St (Maybe (Integer,String))
newRelMem (i,_) = do (_,kv) <- get
                     return $ Map.lookup i kv

getNext :: Integer -> String -> St Integer
getNext n obj = do (i,kv) <- get
                   let p = pred i
                   put (p, Map.insert n (p,obj) kv)
                   return p
