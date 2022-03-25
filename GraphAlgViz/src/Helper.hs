module Helper where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree


--Helper functions for manipulating flags in node labels
--add a boolean flag to the label type using a tuple and a function from nodes to booleans
addFlag :: (LNode a -> Bool) -> LNode a -> LNode (a,Bool)
addFlag p n@(node,label) = (node, (label,p n))

setFlag :: (LNode (a,Bool) -> Bool) -> LNode (a,Bool) -> LNode (a,Bool)
setFlag p n@(node,(label, _)) = (node, (label,p n))

--extract a boolean flag from a node
getFlag :: LNode (a,Bool) -> Bool
getFlag (_,(_,b)) = b

--remove a flag from a node.
removeFlag :: LNode (a,Bool) -> LNode a
removeFlag (n,(l,_)) = (n,l)


incrementFileName :: String -> String
incrementFileName str = show ((read (dropExtension str)) + 1) ++ ".bmp"

dropExtension str = reverse (dropExtension' (reverse str))
dropExtension' [] = []
dropExtension' ('.':xs) = xs
dropExtension' (x:xs) = dropExtension' xs
