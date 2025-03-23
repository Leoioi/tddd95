
import Data.Map as M (Map, empty, unionWith, fromList, (!), keys, member)

data Direction = Up | Down 
type TestCase = [Int]
type DirectionPath = (Int, [Direction])
type DircHist = M.Map Int DirectionPath

emptyHist :: DircHist
emptyHist = M.fromList [(0, (0, []))]

instance Eq Direction where
    (==) :: Direction -> Direction -> Bool
    Up == Up = True
    Down == Down = True
    _ == _ = False

instance Show Direction where
    show :: Direction -> String
    show Up = "Up"
    show Down = "Down"

main :: IO ()
main = do
    content <- readFile "spiderman.in"
    -- content <- getContents
    let lineList = lines content

    let testCases = createTestCases (drop 1 lineList)

    let resultingPaht = map workOut testCases

    let resultingString = map createDirectionString resultingPaht

    mapM_ putStrLn resultingString

createDirectionString :: DirectionPath -> String 
createDirectionString (max,sdirc) | max == (-1) = "IMPOSSIBLE"
                                  | otherwise = map (\dirc -> if dirc == Up then 'U' else 'D') (reverse sdirc)

-- Wrapper 
workOut :: TestCase -> DirectionPath
workOut dircs = recWorkOut dircs emptyHist

-- For each of the directions
recWorkOut :: TestCase -> DircHist -> DirectionPath
recWorkOut [] hist | isGroundEntry = hist ! 0
                   | otherwise  = (-1, [])
                        where isGroundEntry = M.member 0 hist
recWorkOut (x:xs) hist = recWorkOut xs (applyDirection x (M.keys hist) hist)

-- For each key 
applyDirection :: Int -> [Int] -> DircHist -> DircHist
applyDirection _ [] hist = M.empty
applyDirection amount (x:xs) hist | isDownValid && isUpValid = M.fromList [(resultingDown, downResult), (resultingUp, upResult)] +? applyDirection amount xs hist
                                  | isDownValid = M.fromList [(resultingDown, downResult)] +? applyDirection amount xs hist
                                  | isUpValid = M.fromList [(resultingUp, upResult)] +? applyDirection amount xs hist
                                  | otherwise = hist
                            where resultingDown = x - amount
                                  resultingUp = x + amount
                                  (oldMax, oldPath) = (hist M.! x) :: DirectionPath
                                  isDownValid = resultingDown >= 0 :: Bool
                                  isUpValid = resultingUp <= 1000 :: Bool
                                  downResult = (max oldMax resultingDown, Down:oldPath) :: (Int, [Direction])
                                  upResult =  (max oldMax resultingUp, Up:oldPath) :: (Int, [Direction])

-- mapUnification, if we encounter keys that are the same prefer the smallest value
(+?) :: DircHist -> DircHist -> DircHist
map1 +? map2 = M.unionWith (\(maxH1, path1) (maxH2, path2) -> if maxH1 > maxH2 then (maxH2, path2) else (maxH1, path1))  map1 map2

createTestCases :: [String] -> [TestCase]
createTestCases [] = []
createTestCases (_:x:xs) = n : createTestCases xs
                    where n = map read (words x) :: [Int]

