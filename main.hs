import Data.List

data Player = X | O deriving (Eq,Show)
showPlayer :: Player -> String
showPlayer X = "X"
showPlayer O = "O"

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X


data Space = Empty
           | Space Player deriving (Eq,Show)
showSpace :: Space -> Int -> String
showSpace Empty idx = show idx
showSpace (Space p) _ = showPlayer p


type Board = [[Space]]
showBoard :: Board -> String
showBoard [row1, row2, row3] = unlines rows
    where zipWithIndeces = zipWith showSpace
          correctIdxs = [zipWithIndeces row1 [1..], zipWithIndeces row2 [4..], zipWithIndeces row3 [7..]]
          rows = intersperse "-+-+-" $ map (foldl1 (++)) $ map (intersperse "|") correctIdxs

emptyBoard :: Board
emptyBoard = [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]

validIdx :: Board -> Int -> Bool
validIdx b idx =
    case bidx of
        Empty -> True
        _ -> False
    where board = concat b
          bidx = board !! idx

placeMark :: Player -> Board -> Int -> Board
placeMark p b idx = chunksOf 3 newBoard
    where board = concat b
          newBoard = newElement idx board (Space p)

newElement :: Int -> [a] -> a -> [a]
newElement idx list elmnt = prepend ++ [elmnt] ++ append
    where prepend = take idx list
          append  = drop (idx+1) list

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf width list = listChunk:chunksOf width remainingList
        where listChunk = take width list
              remainingList = drop width list

getDiagonals :: Board -> [[Space]]
getDiagonals xs = [left,right]
    where xsFlat = concat xs
          left = (xsFlat !! 0) : (xsFlat !! 4) : (xsFlat !! 8) : []
          right = (xsFlat !! 2) : (xsFlat !! 4) : (xsFlat !! 6) : []
          

data GameStatus = Win Player
                | Continue Player Board

gameLoop :: GameStatus -> IO ()
gameLoop gs =
    case gs of
        Win p ->
            putStrLn $ "Player " ++ (showPlayer p) ++ " wins!"
        Continue p b -> do
            putStrLn $ showBoard b
            idx <- prompt $ "Player " ++ (showPlayer p) ++ ", please choose a valid index: "
            if validIdx b (idx - 1)
            then
                gameLoop $ checkState p $ placeMark p b (idx - 1)
            else
                gameLoop $ Continue p b

startGame :: IO ()
startGame = do
    putStrLn "Welcome to Tic-Tac-Toe!"
    gameLoop (Continue X emptyBoard)


checkState :: Player -> Board -> GameStatus
checkState p b =
    case win of
        True -> Win p
        False -> Continue (otherPlayer p) b
    where diagonals = solveLine p $ getDiagonals b
          solveLine p b = foldl1 (||) $ map (foldl1 (&&)) $ map (map (playerSpc==)) b
            where playerSpc = Space p
          horizontals = solveLine p b
          verticals = solveLine p $ map reverse $ transpose b
          win = diagonals || horizontals || verticals


prompt :: Read a => String -> IO (a)
prompt msg = do
    putStr msg
    fmap read $ getLine

main :: IO ()
main = startGame

-- TODO look for draws