import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, newListArray, newArray, readArray, writeArray)
import Data.Array (Array, listArray, (!))
import Data.Array.MArray (getBounds)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  input <- getContents
  let allLines = lines input
  results <- processTestCases allLines
  mapM_ putStrLn results

processTestCases :: [String] -> IO [String]
processTestCases [] = return []
processTestCases (l:ls) =
  case words l of
    [nStr, mStr, qStr, sStr] ->
      let n = read nStr
          m = read mStr
          q = read qStr
          s = read sStr
      in if n == 0 && m == 0 && q == 0 && s == 0
            then return []
            else do
              let (edgeLines, rest) = splitAt m ls
              let edges = map parseEdge edgeLines
              let adjList = buildAdjList n edges
              let (queryLines, remaining) = splitAt q rest
              let queries = map read queryLines
              let dists = dijkstra n s adjList
              let results = map (formatResult dists) queries
              nextResults <- processTestCases remaining
              return (results ++ nextResults)
    _ -> processTestCases ls -- Skip invalid lines

parseEdge :: String -> (Int, Int, Int)
parseEdge line =
  case map read (words line) of
    [u, v, w] -> (u, v, w)
    _ -> error "Invalid edge format"

buildAdjList :: Int -> [(Int, Int, Int)] -> Array Int [(Int, Int)]
buildAdjList n edges =
  let edgeMap = IM.fromListWith (++) [(u, [(v, w)]) | (u, v, w) <- edges]
      minEdgeMap = IM.map (M.fromListWith min) edgeMap
  in listArray (0, n-1) [M.toList (IM.findWithDefault M.empty u minEdgeMap) | u <- [0..n-1]]

-- | Dijkstra's algorithm using a mutable binary heap implemented in the ST monad.
dijkstra :: Int -> Int -> Array Int [(Int, Int)] -> Array Int Int
dijkstra n start adjList = runST $ do
  dists <- newListArray (0, n-1) (repeat maxBound)
  writeArray dists start 0
  -- Allocate a heap with capacity estimated from the total number of neighbors.
  let cap = (sum [ length (adjList ! i) | i <- [0..n-1] ]) + 100
  heapArr <- newArray (1, cap) (maxBound, -1)  -- dummy initial values
  heapSizeRef <- newSTRef 0
  let heap = (heapSizeRef, heapArr)
  heapPush heap (0, start)
  let loop = do
        mElem <- heapPop heap
        case mElem of
          Nothing -> return ()
          Just (currentDist, u) -> do
            storedDist <- readArray dists u
            if currentDist > storedDist
              then loop
              else do
                forM_ (adjList ! u) $ \(v, w) -> do
                  let newDist = currentDist + w
                  oldDist <- readArray dists v
                  when (newDist < oldDist) $ do
                    writeArray dists v newDist
                    heapPush heap (newDist, v)
                loop
  loop
  freezeArray dists

-- Freeze a mutable array into an immutable one.
freezeArray :: STArray s Int Int -> ST s (Array Int Int)
freezeArray stArr = do
  bounds <- getBounds stArr
  elems <- mapM (readArray stArr) [fst bounds .. snd bounds]
  return $ listArray bounds elems

--------------------------------------------------------------------------------
-- Binary heap implementation (min-heap) for (distance, vertex) pairs
--------------------------------------------------------------------------------

type Heap s = (STRef s Int, STArray s Int (Int, Int))

-- Push an element into the heap.
heapPush :: Heap s -> (Int, Int) -> ST s ()
heapPush (sizeRef, arr) elem = do
  size <- readSTRef sizeRef
  let newSize = size + 1
  writeSTRef sizeRef newSize
  writeArray arr newSize elem
  siftUp arr newSize

-- Restore heap order upwards.
siftUp :: STArray s Int (Int, Int) -> Int -> ST s ()
siftUp arr i = when (i > 1) $ do
  let parent = i `div` 2
  parentElem <- readArray arr parent
  currentElem <- readArray arr i
  when (fst currentElem < fst parentElem) $ do
    swap arr i parent
    siftUp arr parent

-- Swap two elements in the heap.
swap :: STArray s Int (Int, Int) -> Int -> Int -> ST s ()
swap arr i j = do
  temp <- readArray arr i
  a <- readArray arr j
  writeArray arr i a
  writeArray arr j temp

-- Pop the minimum element from the heap.
heapPop :: Heap s -> ST s (Maybe (Int, Int))
heapPop (sizeRef, arr) = do
  size <- readSTRef sizeRef
  if size == 0
    then return Nothing
    else do
      minElem <- readArray arr 1
      lastElem <- readArray arr size
      writeArray arr 1 lastElem
      writeSTRef sizeRef (size - 1)
      newSize <- readSTRef sizeRef
      siftDown arr 1 newSize
      return (Just minElem)

-- Restore heap order downwards.
siftDown :: STArray s Int (Int, Int) -> Int -> Int -> ST s ()
siftDown arr i size = do
  let left = 2 * i
      right = left + 1
  smallest <- if left <= size
              then do
                leftElem <- readArray arr left
                currentElem <- readArray arr i
                return $ if fst leftElem < fst currentElem then left else i
              else return i
  smallest' <- if right <= size
               then do
                 rightElem <- readArray arr right
                 smallestElem <- readArray arr smallest
                 return $ if fst rightElem < fst smallestElem then right else smallest
               else return smallest
  when (smallest' /= i) $ do
      swap arr i smallest'
      siftDown arr smallest' size

--------------------------------------------------------------------------------
-- Formatting the result
--------------------------------------------------------------------------------

formatResult :: Array Int Int -> Int -> String
formatResult dists q
  | dist == maxBound = "Impossible"
  | otherwise = show dist
  where dist = dists ! q