import System.Random
import IOActions

type Map = [[String]]

borders::[Char]
borders = " | _   " 

size::Int
size = 10
maxIndex :: Int
maxIndex = size - 1
numBorders:: Int
numBorders = length borders

initMap :: Map 
initMap  = replicate size (replicate size borders)

disp  :: Show a => [[[a]]] -> IO()
disp x = mapM_ print [ concat y | y <- x ]

prog :: IO () 
prog  = do x <-(loop (initMap))
           disp x

loop  :: Map -> IO Map
loop m = case unresolved m of
            [] -> return m
            ps -> do (i, j) <- randCoord ps
                     c      <- randChar (at m i j)
                     loop (update c i j m)

randCoord    :: [(Int, Int)] -> IO (Int, Int)
randCoord ps  = do x <- (randomRIO(0, (length ps - 1)))
                   return (ps !! x)

randChar    :: [Char] -> IO Char
randChar cs  = if (length cs > 0) then do x <- (randomRIO (0, (length cs - 1))) 
                                          return (cs !! x)
               else return '/'

unresolved  :: Map -> [(Int, Int)]
unresolved m = smallestUnresolved m (numBorders - 2)

smallestUnresolved    :: Map -> Int -> [(Int, Int)]
smallestUnresolved m i = if i == numBorders then getCoords m (not.(has 1))
                         else if (getCoords m (has i) /= []) 
                                then (getCoords m (has i))
                         else smallestUnresolved m (i+1) 


getCoords    :: Map -> ([Char] -> Bool) -> [(Int, Int)]
getCoords m f = [ (i, j) | i <- [0..(size-1)], j <- [0..(size-1)],
                     ( f ( at m i j ) ) ]

has         :: Int -> [a] -> Bool
has i xs = i == length xs

at      :: Map -> Int -> Int -> String
at m i j = m !! i !! j 

update        :: Char -> Int -> Int -> Map -> Map
update c i j m = if i == 0 then (replace (m !! 0) c j) 
                                ++ (elimRow (m !! 1) c j ) ++ (drop 2 m)
                 else if i == maxIndex then (take (i-1) m) 
                                            ++ (elimRow (m !! (i-1)) c j) 
                                            ++ (replace (m !! (size-1)) c j)
                 else (take ( i-1 ) m ) 
                      ++ (elimRow (m !! (i-1)) c j)
                      ++ (replace ( m !! i ) c j)
                      ++ (elimRow (m !! (i+1)) c j)
                      ++ ( drop (i+2) m )

replace      :: [ String ] -> Char -> Int ->[[ String ]]
replace s c i = if i == 0 then [ [[c]] ++ [elim (s !! 1) d] ++ drop 2 s ]
                else if i == maxIndex then [ take ( i-1 ) s
                                            ++ [elim (s !! (i-1)) d ] 
                                            ++ [[c]] ]
                else [ take ( i-1 ) s ++ [elim (s !! (i-1)) d ] 
                     ++ [[c]] ++ [elim (s !! (i+1)) d ] ++ drop (i+2) s ]
                where d = case c of 
                               '|' -> '|'
                               '_' -> ' '
                               _   -> 'c' --anything can go next to a space


elimRow      :: [String] -> Char -> Int -> [[String]]
elimRow s c i = case c of
                '|' -> [take (i) s ++ [elim (s!!i) '_'] ++ drop (i + 1) s]
                '_' -> [take (i) s ++ [elim (s!!i) '|'] ++ drop (i + 1) s]
                _   -> [s]

elim    :: String -> Char -> String
elim s c = [x | x <- s, x /= c]
