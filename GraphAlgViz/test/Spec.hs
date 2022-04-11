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


testStep1ExtendPath =
    let
        graphBefore = mkGraph [(1,("1",Queued,-1)),(2,("2",Queued,-1)),(3,("3",Unexplored,-1)),(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        graphAfter = mkGraph [(1,("1",Queued,-1)),(2,("2",Queued,-1)),(3,("3",Queued,-1)),(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        paramsBefore = SOne 0 [] [(2,("2",Unexplored,-1)),(1,("1",Queued,-1))]
        paramsAfter = SOne 0 [] [(3,("3",Unexplored,-1)),(2,("2",Unexplored,-1)),(1,("1",Queued,-1))] in
    TestCase (assertEqual "Extending Path by going 1 node deeper in dfs," (sccStep' paramsBefore graphBefore) (Right (graphAfter, paramsAfter)))

testStep1ExploreNode =
    let
        graphBefore = mkGraph [(1,("1",Queued,-1)),(2,("2",Queued,-1)),(3,("3",Explored,0)),(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        graphAfter = mkGraph [(1,("1",Queued,-1)),(2,("2",Explored,1)),(3,("3",Explored,0)),(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        paramsBefore = SOne 1 [(3,("3",Unexplored,-1))] [(2,("2",Unexplored,-1)),(1,("1",Queued,-1))]
        paramsAfter = SOne 2 [(2,("2",Unexplored,-1)),(3,("3",Unexplored,-1))] [(1,("1",Queued,-1))] in
    TestCase (assertEqual "fully exploring a node," (sccStep' paramsBefore graphBefore) (Right (graphAfter, paramsAfter)))

testStep1toStep2 =
    let
        graphBefore = mkGraph [(1,("1",Queued,-1)),(2,("2",Explored,1)),(3,("3",Explored,0)),(4,("4",Explored,5)),(5,("5",Explored,4)),(6,("6",Explored,3)),(7,("7",Explored,2))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        graphAfter = mkGraph [(1,("1",Explored,6)),(2,("2",Explored,1)),(3,("3",Explored,0)),(4,("4",Explored,5)),(5,("5",Explored,4)),(6,("6",Explored,3)),(7,("7",Explored,2))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        paramsBefore = SOne 6 [(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1)),(2,("2",Unexplored,-1)),(3,("3",Unexplored,-1))] [(1,("1",Queued,-1))] 
        paramsAfter = STwo [(1,("1",Queued,-1)),(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1)),(2,("2",Unexplored,-1)),(3,("3",Unexplored,-1))] in
    TestCase (assertEqual "Does it step correctly between step 1 and 2" (sccStep' paramsBefore graphBefore) (Right (graphAfter, paramsAfter)))

testStep2toStep3 =
    let
        graphBefore = mkGraph [(1,("1",Explored,6)),(2,("2",Explored,1)),(3,("3",Explored,0)),(4,("4",Explored,5)),(5,("5",Explored,4)),(6,("6",Explored,3)),(7,("7",Explored,2))] [(1,4,"E"),(1,2,"A"),(2,1,"B"),(2,3,"C"),(4,3,"D"),(4,5,"F"),(5,6,"G"),(6,3,"H"),(6,7,"I"),(7,1,"J")]
        graphAfter = mkGraph [(1,("1",Queued,-1)),(2,("2",Unexplored,-1)),(3,("3",Unexplored,-1)),(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1))] [(1,7,"J"),(1,2,"B"),(2,1,"A"),(3,6,"H"),(3,4,"D"),(3,2,"C"),(4,1,"E"),(5,4,"F"),(6,5,"G"),(7,6,"I")]
        paramsBefore = STwo [(1,("1",Queued,-1)),(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1)),(2,("2",Unexplored,-1)),(3,("3",Unexplored,-1))]
        paramsAfter = SThree 5 [(1,("1",Queued,-1)),(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1)),(2,("2",Unexplored,-1)),(3,("3",Unexplored,-1))] [(1,("1",Queued,-1))]  in
    TestCase (assertEqual "Does it step correctly between step 2 and 3" (sccStep' paramsBefore graphBefore) (Right (graphAfter, paramsAfter)))

testStep3ExploringNode =
    let
        graphBefore = mkGraph [(1,("1",Explored,5)),(2,("2",Queued,-1)),(3,("3",Unexplored,-1)),(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Queued,-1))] [(1,7,"J"),(1,2,"B"),(2,1,"A"),(3,2,"C"),(3,6,"H"),(3,4,"D"),(4,1,"E"),(5,4,"F"),(6,5,"G"),(7,6,"I")]
        graphAfter = mkGraph [(1,("1",Explored,5)),(2,("2",Explored,5)),(3,("3",Unexplored,-1)),(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Queued,-1))] [(1,7,"J"),(1,2,"B"),(2,1,"A"),(3,2,"C"),(3,6,"H"),(3,4,"D"),(4,1,"E"),(5,4,"F"),(6,5,"G"),(7,6,"I")]
        paramsBefore = SThree 5 [(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1)),(2,("2",Unexplored,-1)),(3,("3",Unexplored,-1))] [(2,("2",Unexplored,-1)),(7,("7",Unexplored,-1))]
        paramsAfter = SThree 5 [(4,("4",Unexplored,-1)),(5,("5",Unexplored,-1)),(6,("6",Unexplored,-1)),(7,("7",Unexplored,-1)),(3,("3",Unexplored,-1))] [(7,("7",Unexplored,-1))]  in
    TestCase (assertEqual "Does it correctly explore a node in step 3" (sccStep' paramsBefore graphBefore) (Right (graphAfter, paramsAfter)))

testStep3Restarting =
    let
        graphBefore = mkGraph [(1,("1",Explored,5)),(2,("2",Explored,5)),(3,("3",Unexplored,-1)),(4,("4",Explored,5)),(5,("5",Explored,5)),(6,("6",Explored,5)),(7,("7",Explored,5))] [(1,7,"J"),(1,2,"B"),(2,1,"A"),(3,2,"C"),(3,6,"H"),(3,4,"D"),(4,1,"E"),(5,4,"F"),(6,5,"G"),(7,6,"I")]
        graphAfter = mkGraph [(1,("1",Explored,5)),(2,("2",Explored,5)),(3,("3",Queued,6)),(4,("4",Explored,5)),(5,("5",Explored,5)),(6,("6",Explored,5)),(7,("7",Explored,5))] [(1,7,"J"),(1,2,"B"),(2,1,"A"),(3,2,"C"),(3,6,"H"),(3,4,"D"),(4,1,"E"),(5,4,"F"),(6,5,"G"),(7,6,"I")]
        paramsBefore = SThree 5 [(3,("3",Unexplored,-1))] []
        paramsAfter = SThree 6 [] [(3,("3",Queued,6))]  in
    TestCase (assertEqual "Does it correctly restart at a new scc" (sccStep' paramsBefore graphBefore) (Right (graphAfter, paramsAfter)))

testSCCFinishing =
    let
        graphBefore :: Gr ([Char], Flag, Int) [Char]
        graphBefore = mkGraph [(1,("1",Explored,5)),(2,("2",Explored,5)),(3,("3",Explored,6)),(4,("4",Explored,5)),(5,("5",Explored,5)),(6,("6",Explored,5)),(7,("7",Explored,5))] [(1,7,"J"),(1,2,"B"),(2,1,"A"),(3,2,"C"),(3,6,"H"),(3,4,"D"),(4,1,"E"),(5,4,"F"),(6,5,"G"),(7,6,"I")]
        paramsBefore = SThree 6 [] []  in
    TestCase (assertEqual "Does it correctly restart at a new scc" (sccStep' paramsBefore graphBefore) (Left 1))

testsSCC = TestList [
    TestLabel "testStep1ExtendPath" testStep1ExtendPath,
    TestLabel "testStep1ExploreNode" testStep1ExploreNode,
    TestLabel "testStep1toStep2" testStep1toStep2,
    TestLabel "testStep2toStep3" testStep2toStep3,
    TestLabel "testStep3ExploringNode" testStep3ExploringNode,
    TestLabel "testStep3Restarting" testStep3Restarting,
    TestLabel "testSCCFinishing" testSCCFinishing
    ]
tests = TestList [testsBFS, testsDFS,testsSCC]