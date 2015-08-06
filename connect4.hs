import Data.Char
import Data.Text (pack, unpack, strip)

data Board = Board [String]

numCols = 7
numRows = 6
winCount = 4

concatStr [] = []
concatStr (s:ss) = s ++ concatStr ss

blank = "."

instance Show Board where
  show (Board ps) =
    ("  abcdefg\n" ++) $ 
    concatStr $
    [show (numRows-n) ++" " ++  (ps!!((numRows-1)-n)) ++ "\n"
    | n <- [0..(numRows - 1)]]

makeBoard = Board $ take numRows $ repeat $ take numCols $ cycle blank

highest (Board rows) col = minimum $
                           [n | n <- [0..((length column)-1)], column!!n=='.']
                           where column = [r!!col | r <- rows]

dropDisc (Board rows) col piece =
  Board $
  replaceNth
  rows
  (replaceNth (rows!!height) piece col)
  height
  where height = highest (Board rows) col

replaceNth :: [a] -> a -> Int -> [a]
replaceNth [] _ _ = []
replaceNth (x:xs) a 0 = (a:xs)
replaceNth (x:xs) a n = x:(replaceNth xs a (n-1))

toColumn :: Char -> Int
toColumn c = (ord $ toLower c) - ord 'a'

trim =  unpack . strip . pack

splitMove s = fmap trim $ break isSpace s
  
--Plays look like "A  b" where the letters represent columns.
makePlay :: Board -> String -> Board
makePlay b s =
  dropDisc
  (dropDisc b (toColumn (head x)) 'X')
  (toColumn (head o))
  '0'
  where (x,o) = splitMove s

playGame b [] = b
playGame b (move:moves) = playGame (makePlay b move) moves

vertWinSet x n = zip (take 4 $ repeat x) ([n..(n+3)])
vertWinSets x = map (vertWinSet x) [0..(numRows - winCount)]
allVertWinSets = concatMap vertWinSets [0..(numCols-1)]

horizWinSet x n = 

allWinSets = allVertWinSets

discSet _ [] = []
discSet (Board rows) ((x,y):xs) = 
  (rows!!y!!x):discSet( Board rows) xs

allSame [] = True
allSame (_:[]) = True
allSame (a:b:xs) = (a==b) && allSame (b:xs)

playFromFile fileName = do
  plays <- fmap lines $ readFile fileName
  putStrLn $ show $ playGame makeBoard plays

play game = playGame makeBoard game

isWinSet s = allSame s && ((head s) /= (head blank))

isWonGame board = any isWinSet $ map (\x -> discSet board x) allWinSets

winner board = (head . head) $
               filter isWinSet $
               map (\x -> discSet board x) allWinSets

p1 = ["C  d",
      "D  d",
      "D  b",
      "C  f"]

p2 = ["A  d",
      "A  d",
      "A  b",
      "A  f"]

