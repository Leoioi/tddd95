import System.IO
import Data.Map 
import System.IO (readFile)
import Data.List (nub) -- to remove duplicates
import GHC.Generics (Selector(selDecidedStrictness))


-- read file
-- filter words (filter)
-- Switch the key to the front (map)
-- Duplicate pairs should be removed (nub)
-- Create the mapping - need a check to see if the mapping already exists. A map is just a list of pairs 
-- Apply the mapping to get the output 

main :: IO ()
main = do
    -- let pair = (1, "hello")
    content <- readFile "help.in"
    -- content <- getContents
    let wordList = lines content
    let pairNumbers = read (head wordList) :: Int
    let sentences = Prelude.drop 1 wordList -- We should be trying to use new names as much as possible
    let stringPairs = createStringPairs sentences pairNumbers
    let wordPairings = Prelude.map createWordPairs stringPairs
    let keyPairs = Prelude.map ((Prelude.filter) filterKeyWords) wordPairings 

    let notKeyPairs = Prelude.map ((Prelude.filter) (not.filterKeyWords)) wordPairings 
    let isCompleteWordsMask = Prelude.map (\(sentence1, sentence2) -> (sentence1 == sentence2) && not(or (Prelude.map isKey (words sentence1))) && not(or (Prelude.map isKey (words sentence2))) ) stringPairs  
    let isNotCompleteWordsMask = Prelude.map and (Prelude.map (Prelude.map (\(x,y) -> x == y )) notKeyPairs)

    -- the map with out the keys Prelude.map (Prelude.filter (\(x,y) -> isKey x)) keyPairs
    -- flip each pair (Prelude.map (Prelude.map switchPairs)) validMappings

    let validMask1 = Prelude.map (checkValidMapping Data.Map.empty) (Prelude.map (Prelude.filter (\(x,y) -> isKey x)) keyPairs)
    let validMask2 = Prelude.map (checkValidMapping Data.Map.empty) ((Prelude.map (Prelude.filter (\(x,y) -> isKey x)) ((Prelude.map (Prelude.map switchPairs)) keyPairs)))
    let validMask = zipWith (&&) validMask1 validMask2

    let validCase =  Prelude.map (\(testCase, maskValue) -> if maskValue then testCase else [] ) (zip wordPairings validMask)

    let directMappings = Prelude.map (Prelude.filter filterKeyWords) validCase 

    let firstToSecondMap = Prelude.map (createInitialMap True) directMappings
    let secondToFirstMap = Prelude.map (createInitialMap False) directMappings

    let simplifiedFirstToSecondMap = Prelude.map (\(map1, map2) -> simplfiyMappings map1 map2 True) (zip firstToSecondMap secondToFirstMap)
    let simplifiedSecondToFirstMap = Prelude.map (\(map1, map2) -> simplfiyMappings map1 map2 False) (zip firstToSecondMap secondToFirstMap)
    
    let completedSentences = Prelude.map (\(map1, map2, sentence) -> applyMappingToSentence map1 map2 sentence)  (zip3 simplifiedFirstToSecondMap simplifiedSecondToFirstMap (Prelude.map fst stringPairs))
    
    let reallyCompletedSentences = Prelude.map (\(complete,  maskValue) -> if maskValue then complete else "-") (zip completedSentences isNotCompleteWordsMask)

    let reallyREALLYCompleteSentences = Prelude.map (\(complete, original,  maskValue) -> if maskValue then original else complete)  (zip3 reallyCompletedSentences (Prelude.map fst stringPairs) isCompleteWordsMask)

    let reallyREALLYREALLYCompleteSentences = Prelude.map (\str -> if (elem 'A'  str) then "-" else str) reallyREALLYCompleteSentences

    mapM_ putStrLn reallyREALLYREALLYCompleteSentences
    
    -- print $ keyPairs -- Output: 


applyMappingToSentence ::  Data.Map.Map String [String] -> Data.Map.Map String [String] -> String -> String 
applyMappingToSentence mapping1 mapping2 sentence | (Data.Map.null mapping1 ) && (Data.Map.null mapping2) = "-" -- There is no mapping that can be applied so we replace with -
                                        | otherwise = unwords (Prelude.map mapIfKeyWord (words sentence))
                                                where mapIfKeyWord word = if (isKey word) then head (mapping1 ! word) else word

functionWithKey ::  Data.Map.Map String [String] ->  Data.Map.Map String [String] -> Bool -> String -> p -> [String]
functionWithKey mapping1 mapping2 isFirst key value | Prelude.null (path key) = ["x"] 
                                                     | not (doseEveryPathEndTheSame (path key)) = ["A"]
                                                     | otherwise = [((last.head) (path key))]                 
                                                            where path key = dfs key isFirst mapping1 mapping2 []
                                     
doseEveryPathEndTheSame :: Eq a => [[a]] -> Bool
doseEveryPathEndTheSame path = and (Prelude.map ((==) (head (Prelude.map last path))) (Prelude.map last path))


simplfiyMappings :: Data.Map.Map String [String] ->  Data.Map.Map String [String] -> Bool -> Data.Map.Map String [String]
simplfiyMappings mapping1 mapping2 isFirst | isFirst = Data.Map.mapWithKey (functionWithKey mapping1 mapping2 isFirst) mapping1
                                           | not isFirst =  Data.Map.mapWithKey (functionWithKey mapping1 mapping2 isFirst) mapping2
                                               

applyPathToMappings :: Data.Map.Map String [String] -> [String] -> Data.Map.Map String [String]
applyPathToMappings _ [a] =  Data.Map.empty
applyPathToMappings mapping (x:_:path) = Data.Map.insert x [(last path)] mapping

dfs :: String -> Bool -> Data.Map.Map String [String] -> Data.Map.Map String [String] -> [String] -> [[String]]
dfs node isFirst map1 map2 path
    | not (isKey node) = [[node]] -- Base case: return the single node
    | isFirst =  Prelude.map (node :) (Prelude.concatMap recursiveCall nodesMap1NotExplored)
    | otherwise =  Prelude.map (node :) (Prelude.concatMap recursiveCall nodesMap2NotExplored)
  where
    nodesMap1NotExplored = removeIfInPath (map1 ! node) isFirst path
    nodesMap2NotExplored = removeIfInPath (map2 ! node) isFirst path
    recursiveCall x = dfs x (not isFirst) map1 map2 (node : path)


-- We need to remove from the list of potential nodes that we will explore the node that we have already been in given by path, BUT that is dependent on the isFirst bool as 
-- THIS ASSUMES THAT THE FIRST NODE IN THE LIST WILL ALWAYS BE ONE FROM THE FIRST WORD LIST 
removeIfInPath :: [String] -> Bool -> [String] -> [String] 
removeIfInPath potentialNodes isFirst path | isFirst = Prelude.filter (`notElem` secondKeys) potentialNodes
                                           | not isFirst =  Prelude.filter (`notElem` firstKeys) potentialNodes
                                                        where secondKeys | isFirst = [x | (x, i) <- zip path [0..], odd i]
                                                                         | otherwise = [x | (x, i) <- zip path [0..], even i]
                                                              firstKeys | isFirst = [x | (x, i) <- zip path [0..], even i]
                                                                        | otherwise = [x | (x, i) <- zip path [0..], odd i]

createInitialMap :: Bool -> [(String, String)] -> Data.Map.Map String [String] 
createInitialMap isFirst words  = createInitialMapRec words isFirst ks 
            where ks | isFirst = Prelude.map fst (Prelude.filter (\(x,y) -> isKey x) words)
                     | not isFirst = Prelude.map snd (Prelude.filter (\(x,y) -> isKey y) words)


createInitialMapRec :: [(String, String)] -> Bool -> [String] -> Data.Map.Map String [String] 
createInitialMapRec _ _ [] = Data.Map.empty
createInitialMapRec words isFirst (k:ks) = Data.Map.insert k (findNeighborsNodes k words isFirst) (createInitialMapRec words isFirst ks)


findNeighborsNodes :: String -> [(String, String)] -> Bool -> [String]
findNeighborsNodes _ [] _ = []
findNeighborsNodes key ((x1,x2):xs) isFirst | (key == x1) && isFirst = (x2:(findNeighborsNodes key xs isFirst))
                                            | (key == x2) && (not isFirst) = (x1:(findNeighborsNodes key xs isFirst))
                                            | otherwise = findNeighborsNodes key xs isFirst

-- k is the type of the key and v the type of the value 
checkValidMapping :: Data.Map.Map String String -> [(String , String)] -> Bool
checkValidMapping _ [] = True
checkValidMapping mapping ((word1, word2):xs1) | (isKeyInMap word1 mapping ) && ((mapping ! word1) /= word2) && ((not.isKey) word2) = False 
                                               | isKey word2 = checkValidMapping mapping xs1 -- here we ignore if we are trying to add a key, they will not affect the validity check 
                                               | otherwise = checkValidMapping (Data.Map.insert word1 word2 mapping) xs1

-- Create string pairs, could be done with foldr
-- Also we ignore the number lol 
createStringPairs :: [String] -> Int -> [(String, String)]
createStringPairs [] _ = []
createStringPairs (x1:x2:str) n = ((x1,x2) : createStringPairs str n)

createWordPairs :: (String, String) -> [(String, String)]
createWordPairs (xs1, xs2) =  zip (words xs1) (words xs2)

isKeyInMap :: (Ord k) => k -> Data.Map.Map k v -> Bool
isKeyInMap key map = Data.Map.member key map

-- Remove duplicate keys (first element of each pair)
checkDupKeys :: [(String, String)] -> Bool 
checkDupKeys [] = False
checkDupKeys (x:xs) | elem (fst x) (Prelude.map fst xs) = True 
                    | otherwise = checkDupKeys xs 

-- This function will give you the lines of a file as stings in a list
readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    let linesOfFile = lines contents
    return linesOfFile

-- Switch the key to front if necessary
switchPairs :: (String, String) -> (String, String)
switchPairs (word1,word2) = (word2, word1) 

-- Function to check if a word starts with <
isKey :: String -> Bool 
isKey word = ((head word) == '<') && ((last word) == '>')

-- Filtering function to find all the words that matter
filterKeyWords :: (String, String) -> Bool
filterKeyWords (word1, word2) = (isKey word1) || (isKey word2)


