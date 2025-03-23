import Data.List (maximumBy, find, sortBy, sort)
import Data.Ord (comparing)
import Data.Maybe (isNothing, isJust)
import Distribution.TestSuite (Test)
import GHC.Real (infinity)

type Interval = (Double,Double) -- Declare an interval type to help with readability 
type TestCase = (Interval, [Interval]) -- Respectively the interval we want to cover the number of intervals and said intervals 
type TestCaseWIndices = (Interval, [(Interval, Int)]) -- Respectively the interval we want to cover the number of intervals and said intervals 


type Result = (Int, [Int])

emptyResults :: Result
emptyResults = (0, [])

main :: IO ()
main = do
    content <- readFile "interval.in"
    -- content <- getContents
    let wordList = lines content

    let allTestCases = createTestCasesFromInput wordList

    let results = map findBestNumberOfIntervals allTestCases

    mapM_ putStrLn (concatMap formatResults results)

filterOutPossibleBadIntervals :: TestCaseWIndices -> TestCaseWIndices
filterOutPossibleBadIntervals (ival,[]) = (ival,[])
filterOutPossibleBadIntervals ((a,b),((ia,ib),i):xs) | ib < a = filterOutPossibleBadIntervals ((a,b),xs)
                                                     | otherwise = ((a,b),((ia,ib),i):xs)

sortIntervalsInCase :: TestCaseWIndices -> TestCaseWIndices
sortIntervalsInCase (ival, sival) =  (ival, sortBy (\((a1,b1),i) ((a2,b2),j) -> compare a1 a2) sival) -- Sort by interval starting point 

formatResults :: Maybe (Int, [Int]) -> [String]
formatResults mr
    | isNothing mr = ["impossible"]
    | isJust mr =
        let Just (n, inds) = mr
        in show n : [drop 1 (concatMap (\x -> ' ' : show x) inds)]

findBestNumberOfIntervals :: TestCase -> Maybe Result -- Maybe filter out any bad intervals here?
findBestNumberOfIntervals (ival, xs) =  findBestNumberOfIntervalsWi (sortIntervalsInCase (ival, zip xs [0..]))

findBestNumberOfIntervalsWi :: TestCaseWIndices -> Maybe Result
findBestNumberOfIntervalsWi ((a,b), sival) | null sival || null intervalsOverLappingWithStart = Nothing
                                           | b <= bb = Just (1, [mIndex])
                                           | otherwise = findBestNumberOfIntervalsWi ((bb, b), leftoverIntervals) >>= (\(num, inds) -> Just (1 + num, mIndex:inds))
                                    where intervalsOverLappingWithStart = getBestInterval sival a (head sival)
                                          ((ab, bb), mIndex) = head intervalsOverLappingWithStart -- best interval 
                                          leftoverIntervals = drop 1 intervalsOverLappingWithStart

getBestInterval :: [(Interval, Int)] -> Double -> (Interval, Int) -> [(Interval, Int)]
getBestInterval [] _ best = [best]
getBestInterval (((a,b),i):xs) n best | n >= a = if (\((a1,b1),_) ((a2,b2),_) -> b1 <= b2) best ((a,b),i) then getBestInterval xs n ((a,b),i) else getBestInterval xs n best
                                      | otherwise = best:((a,b),i):xs

isCovered :: Double -> (Interval, Int) -> Bool
isCovered n ((a,b),_) = (a <= n) && (n <= b)

isIntervalInside  ::  Double -> (Interval,Int) -> Bool
isIntervalInside n ((a,b),i) = n < b  -- note strictly smaller...

biggestIntervalUncoveredWIndex :: Double -> [(Interval, Int)]  -> (Interval, Int)
biggestIntervalUncoveredWIndex n  = maximumBy (\((a1,b1),_) ((a2,b2), _) -> compare b1 b2)

biggestIntervalWIndex :: [(Interval, Int)] -> (Interval, Int)
biggestIntervalWIndex = maximumBy (\(ia,_) (ib, _) -> compare (iLength ia) (iLength ib))

iLength :: Interval -> Double
iLength (a,b) = abs (a - b)

intervalFromInput:: String -> Interval
intervalFromInput str = ((read.head.words) str,  (read.last.words) str)

createTestCasesFromInput :: [String] -> [TestCase]
createTestCasesFromInput [] = []
createTestCasesFromInput (x:sn:xs) = (ab, coveringIntervals) : createTestCasesFromInput (drop n xs)
                                where ab = intervalFromInput x
                                      n = read sn :: Int
                                      coveringIntervals = map intervalFromInput (take n xs)