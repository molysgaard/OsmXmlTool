{-# LANGUAGE TypeSynonymInstances #-}
module GenXml where

import qualified Data.Map as Map
import Text.XML.HaXml
import Text.XML.HaXml.Pretty

import GenNew

class Print str where
  pp :: str -> String

instance Print String where
  pp s = init . (drop 1) $ show s

instance Print Integer where
  pp i = show i

instance Print Double where
  pp d = show d

genAttr :: (Print a, Print b) => a -> b -> Attribute
genAttr k v = (N (pp k), AttValue [Left (pp v)])

genTags :: Tags -> [Content String]
genTags m = map genTag $ Map.toList m

genTag :: (String, String) -> Content String
genTag (k,v) = CElem (Elem (N "tag") [genAttr "k" k, genAttr "v" v] []) ""

genNode :: Node -> Content String
genNode node
  | nId node < 0 = CElem (Elem (N "node") [genAttr "id" (nId node), genAttr "lat" (lat node), genAttr "lon" (lon node), genAttr "action" "create"] (genTags (nTags node))) ""
  | otherwise  = CElem (Elem (N "node") [genAttr "id" (nId node)] (genTags (nTags node))) ""

genNodeRef :: Integer -> Content String
genNodeRef n = CElem (Elem (N "nd") [genAttr "ref" n] []) ""

genWay :: Way -> Content String
genWay way
  | wId way < 0 = CElem (Elem (N "way") [genAttr "id" (wId way), genAttr "action" "create"] ((genTags (wTags way)) ++ (map genNodeRef (wMembs way)))) ""
  | otherwise = CElem (Elem (N "way") [genAttr "id" (wId way)] ((genTags (wTags way)) ++ (map genNodeRef (wMembs way)))) ""

genRelationRef :: (Integer, String) -> Content String
genRelationRef (n,obj) = CElem (Elem (N "member") [genAttr "ref" n, genAttr "type" obj] []) ""

genRelation :: Relation -> Content String
genRelation rel
  | rId rel < 0 = CElem (Elem (N "relation") [genAttr "id" (rId rel), genAttr "action" "create"] ((genTags (rTags rel)) ++ (map genRelationRef (rMembs rel)))) ""
  | otherwise = CElem (Elem (N "relation") [genAttr "id" (rId rel)] ((genTags (rTags rel)) ++ (map genRelationRef (rMembs rel)))) ""

genContent :: File -> [Content String]
genContent (File ns ws rs) = concat [map genNode ns, map genWay ws, map genRelation rs]

pro = Prolog (Just (XMLDecl "1.0" (Just (EncodingDecl "UTF-8")) Nothing)) [] Nothing []

genXml :: File -> Document String
genXml f = Document pro emptyST (Elem (N "osm") [genAttr "version" "0.6"] (genContent f)) []
