import System.IO
import Data.List
import Data.Char
import Data.Array
import Debug.Trace
import qualified Data.Map.Lazy as M

type Meme = [String]
type User = String
type Message = String
type ChatLog = M.Map User [Message]

dir :: FilePath
dir = "haskibot/" ++ "#elajjaz-2016-05-12"

main :: IO ()
main = withFile dir ReadMode (\hdl -> hGetContents hdl >>= printMemes . recognizeMemes . parseLog ) 

printMemes :: [Meme] -> IO ()
printMemes [] = return ()
printMemes (m:ms) = do print m; putStrLn ""; printMemes ms

parseLog :: String -> ChatLog
parseLog =  foldl parseEntry M.empty . lines
  where parseEntry :: ChatLog -> String -> ChatLog
        parseEntry log e = let (username, message)  = (takeWhile (/=',') e, tail . dropWhile (/=',') $ e) 
                           in M.insertWith (++) username [message] log

-- For Each User, Partition Messages By Similarity (Edit Distance). Each partition is called a Meme Candidates
-- Filter Meme Candidates of size 1
-- Partition all Meme Candidates By Similarity (Smallest Edit Distance among messages)
-- Filter partitions of size 1
-- For each Meme Candidate Partition, Merge all Meme Candidates together
-- Remaining list of Meme Candidates are considered Memes

recognizeMemes :: ChatLog -> [Meme]
recognizeMemes = 
    map concat . 
    groupBy memesAreSimilar . 
    filter ((>=user_repetition_threshold) . length) . 
    concat . 
    map (groupBy messagesAreSimilar) . 
    M.elems
  where
        messagesAreSimilar :: Message -> Message -> Bool
        messagesAreSimilar m1 m2 = editDistance m1 m2 <= message_similarity_threshold
        
        memesAreSimilar :: Meme -> Meme -> Bool
        memesAreSimilar m1 m2 = 
            (minimum [editDistance s1 s2 | s1 <- m1, s2 <- m2]) <= meme_similarity_threshold ||
            elem True [(s1 `isInfixOf` s2) || (s2 `isInfixOf` s1) | s1 <- m1, s2 <- m2]

        -- Algorithm Parameters
        meme_similarity_threshold       = 5
        message_similarity_threshold    = 2
        user_repetition_threshold       = 2

editDistance :: String -> String -> Int
editDistance [] s = length s
editDistance s [] = length s
editDistance string1 string2 = memoTable ! (n,m)    
  where
    [n,m] = map length [string1, string2]
    [s1,s2] = map (\(s, l) -> listArray (1, l) . map toLower $ s) [(string1, n), (string2, m)]
    
    memoTable :: Array (Int,Int) Int
    memoTable = array ((0,0), (n,m)) $ [( (i, j), f i j)   | i <- [0 .. n], j <- [0 .. m]]
        where 
            f i j 
                | i == 0    = j
                | j == 0    = i
                | otherwise =
                    minimum 
                    [
                    memoTable ! (i-1,j-1)  + if s1 ! i == s2 ! j then 0 else 1,
                    memoTable ! (i-1,j)    + 1,
                    memoTable ! (i,j-1)    + 1
                    ]

