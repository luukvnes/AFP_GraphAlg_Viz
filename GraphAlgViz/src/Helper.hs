module Helper where

import Data.List
import Data.List.Split
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import System.Directory
import System.IO
import Control.Monad
import Graph


-- | 'Flag' is a type for storing the status of a 'LNode' during execution of algorithms,
--  with accompanying helper functions for manipulating flags in node labels
data Flag = Unexplored
          | Explored
          | Queued
          | Goal
          deriving (Show, Eq, Ord)

-- | add a flag to the label type using a tuple and a function from nodes to flags
addFlag :: (LNode a -> Flag) -> LNode a -> LNode (a,Flag)
addFlag p n@(node,label) = (node, (label,p n))

-- | set the flag of a node, using a function that computes the new flag from the old node.
setFlag :: (LNode (a,Flag) -> Flag) -> LNode (a,Flag) -> LNode (a,Flag)
setFlag p n@(node,(label, _)) = (node, (label,p n))

-- | extracts a flag from a node
getFlag :: LNode (a,Flag) -> Flag
getFlag (_,(_,f)) = f

-- | removes a flag from a node.
removeFlag :: LNode (a,Flag) -> LNode a
removeFlag (n,(l,_)) = (n,l)

-- |'addFlagSCC' takes a 'LNode' and a function that can produce a 'Flag' given that 'LNode'. It then adds that 'Flag' to the 'LNode' and a dummy int representing its component\stack location
addFlagSCC :: (LNode a -> Flag) -> LNode a -> LNode (a,Flag, Int)
addFlagSCC p n@(node,label) = (node, (label,p n,-1))

-- |'addFlagSCC' takes a 'LNode' and a extracts its 'Flag'
getFlagSCC :: LNode (a,Flag, Int) -> Flag
getFlagSCC (_,(_,f,_)) = f

-- |'removeFlagSCC' takes a 'LNode' returns the node without its 'Flag' and component/stack id
removeFlagSCC :: LNode (a,Flag, Int) -> LNode a
removeFlagSCC (n,(l,_,_)) = (n,l)


-- |'fstT' takes a triplet and returns its first value
fstT :: (a, b, c) -> a
fstT (a, _, _) = a

-- |'incrementFileName' takes a string in the shape of NNNN.ext (0063.bmp) a file extension and returns the incremented file name (0064.bmp)
incrementFileName :: String -> String -> String
incrementFileName str extension = zeros ++ resNString ++ extension
    where
        resNString :: String
        resNString = show (read (dropExtension str) + 1)
        zeros      :: String
        zeros      = replicate (4 - length resNString) '0'

-- |'incrementFileName' takes a file name and drops its extension
dropExtension :: String -> String
dropExtension []      = []
dropExtension ('.':_) = []
dropExtension (x:xs)  = x : dropExtension xs

-- |'incrementFolderName' takes a string in the shape of NNNN (0063) and returns the incremented folder name (0064)
incrementFolderName :: String -> String
incrementFolderName str = zeros ++ resNString
    where
        resNString :: String
        resNString = show (read (dropExtension str) + 1)
        zeros      :: String
        zeros      = replicate (4 - length resNString) '0'

-- |'createFolderStructure' creates all the required folders where images and gifs are stored.
createFolderStructure :: IO ()
createFolderStructure = do
    exists <- doesDirectoryExist "resultFolder"
    unless exists $ createDirectory "resultFolder"
    exists <- doesDirectoryExist "resultFolder/gifResults"
    unless exists $ createDirectory "resultFolder/gifResults"
    exists <- doesDirectoryExist "resultFolder/ImageFolders"
    unless exists $ createDirectory "resultFolder/ImageFolders"
    dirs   <- listDirectory "resultFolder/ImageFolders"
    let isDirEmpty =  null dirs
    if isDirEmpty then createDirectory "resultFolder/ImageFolders/0000" else createDirectory $ "resultFolder/ImageFolders/" ++ incrementFolderName (last (sort dirs))


-- |'getGifPath' takes user input and returns the path for a gif with default 'retrieveDefaultGif' if the user enters nothing
getGifPath :: IO String
getGifPath = do
    line <- getLine
    if line == "" then retrieveDefaultGif else return line

-- |'retrieveDefaultGif' returns the default gif path, which is the path of the latest default gif incremented by 1.
retrieveDefaultGif :: IO [Char]
retrieveDefaultGif = do
    gifFiles <- listDirectory "resultFolder/gifResults"
    let lastFile = if null gifFiles then "0000.gif" else last (sort gifFiles)
    return $ "resultFolder/gifResults/" ++ incrementFileName lastFile ".gif"

-- |'getGraph' takes user input and returns the path for the used graph with default graphs/default.txt
getGraph :: IO (Gr String String)
getGraph = do
    line     <- getLine
    let location = if line == "" then "graphs/default.txt" else line
    handle   <- openFile location ReadMode
    contents <- hGetContents handle
    return $ parseGraph contents

-- |'getSize' takes user input and returns the size of the resulting image/gif with default (250,400)
getSize :: IO (Double, Double)
getSize = do
    line <- getLine
    if line == "" then return (250 :: Double,400 :: Double) else return (read line)
