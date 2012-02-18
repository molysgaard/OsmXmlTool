module ParseXml where

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import qualified Text.XML.HaXml.Pretty as Pp

import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import GenNew

--parseXml :: String -> File
parseXml xmlString =
    let (Document _ _ root _) = xmlParse "DUMMY" xmlString
        rootElem = CElem root noPos
        xmlNodes = (tag "osm" /> tag "node") rootElem
        xmlWays  = (tag "osm" /> tag "way") rootElem
        xmlRels  = (tag "osm" /> tag "relation") rootElem
    in File (map genOsmNode xmlNodes) (map genOsmWay xmlWays) (map genOsmRelation xmlRels)

genOsmTag :: [Content Posn] -> Tags
genOsmTag tags = Map.fromList . catMaybes $ map help tags
  where help (CElem (Elem (N "tag") attrs _) _) = Just (getAttr "k" attrs, getAttr "v" attrs)
        help _ = Nothing

getAttr :: String -> [Attribute] -> String
getAttr key [] = "" --error $ "Could not find attribute: " ++ show key
getAttr key ((N k, AttValue [Left v]):kvs)
  | key == k = v
  | otherwise = getAttr key kvs
getAttr key _ = ""

genOsmWayMembs :: [Content Posn] -> [Integer]
genOsmWayMembs nodes = map (\(CElem (Elem (N "nd") attrs _) _) -> read $ getAttr "ref" attrs) nodes

genOsmRelationMembs :: [Content Posn] -> [(Integer,String)]
genOsmRelationMembs membs = map (\(CElem (Elem (N "member") attrs _) _) -> (read (getAttr "ref" attrs), getAttr "type" attrs)) membs

genOsmNode :: Content Posn -> Node
genOsmNode (CElem (Elem n attrs tags) _) = Node { nId = read $ getAttr "id" attrs
                                              --, nVer = read $ getAttr "version" attrs
                                              , lat = read $ getAttr "lat" attrs
                                              , lon = read $ getAttr "lon" attrs
                                              , nTags = genOsmTag tags }

genOsmWay :: Content Posn -> Way
genOsmWay way@(CElem (Elem n attrs _) _) = Way { wId = read $ getAttr "id" attrs
                                               --, wVer = read $ getAttr "version" attrs
                                               , wMembs = genOsmWayMembs ((tag "way" /> tag "nd") way)
                                               , wTags = genOsmTag ((tag "way" /> tag "tag") way) }
genOsmRelation :: Content Posn -> Relation
genOsmRelation rel@(CElem (Elem n attrs _) _) = Relation { rId = read $ getAttr "id" attrs
                                                         --, rVer = read $ getAttr "version" attrs
                                                         , rMembs = genOsmRelationMembs ((tag "relation" /> tag "member") rel)
                                                         , rTags = genOsmTag ((tag "relation" /> tag "tag") rel) }

t = unlines ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
             "<osm",
             "  ><node id=\"-1\" lat=\"0.0\" lon=\"0.0\" action=\"create\"",
             "    ><tag k=\"name\" v=\"alpha\"/></node",
             "  ><node id=\"-2\" lat=\"1.0\" lon=\"0.0\" action=\"create\"",
             "    ><tag k=\"name\" v=\"bravo\"/></node",
             "  ><node id=\"-3\" lat=\"0.0\" lon=\"1.0\" action=\"create\"",
             "    ><tag k=\"name\" v=\"charlie\"/></node",
             "  ><way id=\"-4\" action=\"create\"",
             "    ><tag k=\"name\" v=\"delta\"",
             "    /><nd ref=\"-1\"",
             "    /><nd ref=\"-2\"",
             "    /><nd ref=\"-3\"/></way",
             "  ><relation id=\"-5\" action=\"create\"",
             "    ><tag k=\"name\" v=\"echo\"",
             "    /><member ref=\"-1\" type=\"node\"",
             "    /><member ref=\"-3\" type=\"node\"",
             "    /><member ref=\"-4\" type=\"way\"/></relation></osm>"]
