module Helper where

import Data.List
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

--Helper functions for manipulating flags in node labels for SCC
--add a boolean flag to the label type using a tuple and a function from nodes to booleans
addFlagSCC :: (LNode a -> Flag) -> LNode a -> LNode (a,Flag, Int)
addFlagSCC p n@(node,label) = (node, (label,p n,-1))
--extract a boolean flag from a node
getFlagSCC :: LNode (a,Flag, Int) -> Flag
getFlagSCC (_,(_,f,_)) = f

--remove a flag from a node.
removeFlagSCC :: LNode (a,Flag, Int) -> LNode a
removeFlagSCC (n,(l,_,_)) = (n,l)

fstT :: (a, b, c) -> a
fstT (a, b, c) = a

incrementFileName :: String -> String -> String
incrementFileName str extension = zeros ++ resNString ++ extension
    where
        resNString :: String
        resNString = show (read (dropExtension str) + 1)
        zeros :: String
        zeros = replicate (4 - length resNString) '0'

-- dropExtension str = reverse (dropExtension' (reverse str))
dropExtension [] = []
dropExtension ('.':xs) = []
dropExtension (x:xs) = x : dropExtension xs

incrementFolderName :: String -> String
incrementFolderName str = zeros ++ resNString
    where
        resNString :: String
        resNString = show (read (dropExtension str) + 1)
        zeros :: String
        zeros = replicate (4 - length resNString) '0'

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
    if isDirEmpty then createDirectory "resultFolder/ImageFolders/0000" else createDirectory $ "resultFolder/ImageFolders/" ++ incrementFolderName (last (sort dirs))

getGifPath :: IO String
getGifPath = do
    line <- getLine
    if line == "" then retrieveDefaultGif else return line

retrieveDefaultGif :: IO [Char]
retrieveDefaultGif = do
    gifFiles <- listDirectory "resultFolder/gifResults"
    let lastFile = if null gifFiles then "0000.gif" else last (sort gifFiles)
    return $ "resultFolder/gifResults/" ++ incrementFileName lastFile ".gif"

getGraph :: IO (Gr String String)
getGraph = do
    line <- getLine
    let location = if line == "" then "graphs/default.txt" else line
    handle <- openFile location ReadMode
    contents <- hGetContents handle
    return $ parseGraph contents
