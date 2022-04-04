module Helper where

import Data.List.Split
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import System.Directory
import System.IO
import Control.Monad
import Graph
data Flag = Unexplored
          | Explored
          | Queued
          deriving (Show, Eq, Ord)

--Helper functions for manipulating flags in node labels
--add a boolean flag to the label type using a tuple and a function from nodes to booleans
addFlag :: (LNode a -> Flag) -> LNode a -> LNode (a,Flag)
addFlag p n@(node,label) = (node, (label,p n))

setFlag :: (LNode (a,Flag) -> Flag) -> LNode (a,Flag) -> LNode (a,Flag)
setFlag p n@(node,(label, _)) = (node, (label,p n))

--extract a boolean flag from a node
getFlag :: LNode (a,Flag) -> Flag
getFlag (_,(_,f)) = f

--remove a flag from a node.
removeFlag :: LNode (a,Flag) -> LNode a
removeFlag (n,(l,_)) = (n,l)


incrementFileName :: String -> String -> String
incrementFileName str extension = show (read (dropExtension str) + 1) ++ extension

dropExtension str = reverse (dropExtension' (reverse str))
dropExtension' [] = []
dropExtension' ('.':xs) = xs
dropExtension' (x:xs) = dropExtension' xs

incrementFolderName :: String -> String
incrementFolderName str = show (read str +1)

createFolderStructure :: IO ()
createFolderStructure = do
    exists <- doesDirectoryExist "resultFolder"
    unless exists $ createDirectory "resultFolder"
    exists <- doesDirectoryExist "resultFolder/gifResults"
    unless exists $ createDirectory "resultFolder/gifResults"
    exists <- doesDirectoryExist "resultFolder/ImageFolders"
    unless exists $ createDirectory "resultFolder/ImageFolders"
    dirs <- listDirectory "resultFolder/ImageFolders"
    let isDirEmpty =  null dirs
    if isDirEmpty then createDirectory "resultFolder/ImageFolders/1" else createDirectory $ "resultFolder/ImageFolders/" ++ incrementFolderName (head dirs)

getGifPath :: IO String
getGifPath = do
    line <- getLine
    if line == "" then retrieveDefaultGif else return line

retrieveDefaultGif :: IO [Char]
retrieveDefaultGif = do
    gifFiles <- listDirectory "resultFolder/gifResults"
    let lastFile = if null gifFiles then "0.gif" else head gifFiles
    return $ "resultFolder/gifResults/" ++ incrementFileName lastFile ".gif"

getGraph :: IO (Gr String String)
getGraph = do
    line <- getLine
    let location = if line == "" then "graphs/default.txt" else line
    handle <- openFile location ReadMode
    contents <- hGetContents handle
    return $ parseGraph contents
