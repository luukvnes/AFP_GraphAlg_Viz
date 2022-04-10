import Test.HUnit
import Algorithms
import Helper
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

main :: IO ()
main = do
    cs <- runTestTT tests
    print cs

testbasicBFS =
    let
        graphBefore = mkGraph [(1,("1",Explored)),(2,("2",Queued)),(3,("3",Unexplored)),(4,("4",Queued)),(5,("5",Unexplored)),(6,("6",Unexplored)),(7,("7",Unexplored))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        graphAfter = mkGraph [(1,("1",Explored)),(2,("2",Explored)),(3,("3",Queued)),(4,("4",Queued)),(5,("5",Unexplored)),(6,("6",Unexplored)),(7,("7",Unexplored))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        paramsBefore = (\n@(i,l) -> l == "7", [(2,("2",Queued)),(4,("4",Unexplored))])
        paramsAfter = (\n@(i,l) -> l == "7", [(4,("4",Unexplored)),(3,("3",Unexplored))]) in
    TestCase (assertEqual "basic BFS step" (bfsStep' paramsBefore graphBefore) (Right (graphAfter, paramsAfter)))

testBFSFoundNode =
    let
        graphBefore = mkGraph [(1,("1",Explored)),(2,("2",Explored)),(3,("3",Explored)),(4,("4",Explored)),(5,("5",Explored)),(6,("6",Explored)),(7,("7",Queued))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        paramsBefore = (\n@(i,l) -> l == "7", [(7,("7",Queued))]) in
    TestCase (assertEqual "testing if BFS works correctly when the noude is found" (bfsStep' paramsBefore graphBefore) (Left (Just (7, "7"))))

testBFSFinishedWithoutFind =
    let
        graphBefore = mkGraph [(1,("1",Explored)),(2,("2",Explored)),(3,("3",Explored)),(4,("4",Explored)),(5,("5",Explored)),(6,("6",Explored))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H")]
        paramsBefore = (\n@(i,l) -> l == "7", []) in
    TestCase (assertEqual "testing if BFS works correctly when the noude is not found" (bfsStep' paramsBefore graphBefore) (Left Nothing))


testsBFS = TestList [
    TestLabel "testbasicBFS" testbasicBFS,
    TestLabel "testbfsFoundNode" testBFSFoundNode,
    TestLabel "testbfsFinishedWithoutFind" testBFSFinishedWithoutFind
    ]


testbasicDFS =
    let
        graphBefore = mkGraph [(1,("1",Explored)),(2,("2",Queued)),(3,("3",Unexplored)),(4,("4",Queued)),(5,("5",Unexplored)),(6,("6",Unexplored)),(7,("7",Unexplored))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        graphAfter = mkGraph [(1,("1",Explored)),(2,("2",Explored)),(3,("3",Queued)),(4,("4",Queued)),(5,("5",Unexplored)),(6,("6",Unexplored)),(7,("7",Unexplored))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        paramsBefore = (\n@(i,l) -> l == "7", [(2,("2",Queued)), (4,("4",Unexplored))])
        paramsAfter = (\n@(i,l) -> l == "7", [(3,("3",Unexplored)),(4,("4",Unexplored))]) in
    TestCase (assertEqual "basic DFS step," (dfsStep' paramsBefore graphBefore) (Right (graphAfter, paramsAfter)))

testDFSFoundNode =
    let
        graphBefore = mkGraph [(1,("1",Explored)),(2,("2",Explored)),(3,("3",Explored)),(4,("4",Explored)),(5,("5",Explored)),(6,("6",Explored)),(7,("7",Queued))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        paramsBefore = (\n@(i,l) -> l == "7", [(7,("7",Queued))]) in
    TestCase (assertEqual "testing if DFS works correctly when the noude is found" (dfsStep' paramsBefore graphBefore) (Left (Just (7, "7"))))

testDFSFinishedWithoutFind =
    let
        graphBefore = mkGraph [(1,("1",Explored)),(2,("2",Explored)),(3,("3",Explored)),(4,("4",Explored)),(5,("5",Explored)),(6,("6",Explored))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H")]
        paramsBefore = (\n@(i,l) -> l == "7", []) in
    TestCase (assertEqual "testing if DFS works correctly when the noude is not found" (dfsStep' paramsBefore graphBefore) (Left Nothing))


testsDFS = TestList [
    TestLabel "testbasicBFS" testbasicDFS,
    TestLabel "testbfsFoundNode" testDFSFoundNode,
    TestLabel "testbfsFinishedWithoutFind" testDFSFinishedWithoutFind
    ]

tests = TestList [testsBFS, testsDFS]