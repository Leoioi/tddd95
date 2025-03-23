import Data.Array (listArray, Array, (!), array, (//))
import Data.ByteString.Builder.Prim (emptyB)

type Item = (Int, Int)
type TestCase = (Int, [Item]) -- This test case declaration is for the weight limit and hte items respectivly

-- A BagConfiguration will represent the value of some specific configuration of items and the indices of those items
type BagConfiguration = (Int, [Int])

-- This will be used as our matrix, it has constant time complexity for access 
type Matrix = Array (Int, Int) BagConfiguration

main :: IO ()
main = do
    content <- readFile "knapsack.in"
    -- content <- getContents
    let wordList = lines content

    let allTestCases = createTestCasesFromInput wordList

    let results = map knapsack allTestCases

    let printableResults = concatMap formatResults results

    mapM_ putStrLn printableResults


formatResults :: BagConfiguration -> [String]
formatResults (_,xs) = [stringNumberOfIndices, stringIndices]
                    where stringIndices = drop 1 (concatMap (((' ' :).show) . (\n -> n-1)) xs)
                          stringNumberOfIndices = (show.length) xs

createTestCasesFromInput :: [String] -> [TestCase]
createTestCasesFromInput [] = []
createTestCasesFromInput (x:sline) = (capacity, allItems) : createTestCasesFromInput (drop numberOfItems sline)
                        where firstLine = map read (words x) :: [Int]
                              capacity = head firstLine
                              numberOfItems = last firstLine
                              allItems = map ((\[a, b] -> (a, b)) . map read . words) (take numberOfItems sline) :: [(Int, Int)]



knapsack :: TestCase -> BagConfiguration
knapsack (w,items) = completeDpMatrix ! (n, w)
    where
        n = length items -- 
        itemsList = listArray (1, n) items

        dp :: Matrix
        dp = array ((0, 0), (n, w))
            [((i, j), (0, [])) | i <- [0..n], j <- [0..w]] -- fill with zeros

        knapsackDP :: Int -> Int -> Matrix -> BagConfiguration
        knapsackDP 0 _ _ = (0,[])
        knapsackDP _ 0 _ = (0,[])
        knapsackDP i j dp
            | itemWeight > j = dp ! (i-1, j) -- We don't have enough capacity to add this item, 
            | otherwise = maxValue (dp ! (i-1, j))  (dp ! (i-1, j-itemWeight) +! (itemValue, [i])) -- We can now try to add another item but only if there is not some better configuration
            where (itemValue, itemWeight) = itemsList ! i

        fori1 :: Int -> Matrix -> Matrix 
        fori1 i m | i > n = m
                  | otherwise = fori1 (i+1) $! (fori2 0 m)
                where fori2 :: Int -> Matrix -> Matrix 
                      fori2 j m | j > w = m
                                | otherwise = fori2 (j+1) $! (m // [((i, j), knapsackDP i j m)])

        completeDpMatrix = fori1 0 dp :: Matrix

        maxValue :: BagConfiguration -> BagConfiguration -> BagConfiguration
        maxValue (v1,is1) (v2,is2) | v1 > v2 = (v1,is1)
                                   | otherwise = (v2,is2)

        (+!) :: BagConfiguration -> BagConfiguration -> BagConfiguration
        (v1,is1) +! (v2,is2) = (v1 + v2, is1 ++ is2)

  