main :: IO ()
main = do

    content <- readFile ".in"
    -- content <- getContents
    let wordList = lines content

    print $ "hello worlds"