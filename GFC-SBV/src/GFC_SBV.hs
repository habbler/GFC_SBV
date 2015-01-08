{-# LANGUAGE TupleSections #-}
module GFC_SBV where

import Data.Array
import Data.List
import Data.Maybe

import Data.SBV
import Control.Applicative
import Control.Monad (when)

type Direction = SWord8

type Colour = SWord8
type Distance = SWord8

type Elem = (Direction, Colour, Distance)

unDir (dir,_,_) = dir

type Row = [Elem]

type Board = [Row]

type Coord = (Int, Int)

-- | Find the coord in direction d from give coord
updateCoord :: Int -> Coord -> Direction -> Maybe Coord
updateCoord uBound (x,y) direction = case direction of 
                                        0 -> cond (y < uBound) (x,y+1) -- N
                                        1 -> cond (x < uBound) (x+1,y) -- E
                                        2 -> cond (y > 0     ) (x,y-1) -- S  
                                        3 -> cond (x > 0     ) (x-1,y) -- W
     where cond c v = if c then Just v else Nothing
     
oppDirection :: Direction -> Direction
oppDirection dir = ite (dir .== 4)  4 $  -- No direction
                   let newDir = dir + 2 in
                   ite (newDir .> 3) (newDir - 4) newDir
        

matchDir :: [(Direction,Elem)] -> Direction -> Elem
matchDir [] _ = (4,98,99)
matchDir (x@(dir,elem):xs) direct = ite (direct .== dir) elem (matchDir xs direct)                                      
       
exactlyOne :: [SBool] -> SBool
exactlyOne [] = false            
exactlyOne (t1:tRest) = t1 &&& (bAnd $ map bnot tRest) ||| (bnot t1 &&& exactlyOne tRest)                           

checkElement :: Array Coord Elem -> (Coord,Elem) -> SBool
checkElement arr (coord, (dir,color, dist)) = 
    if coord == (0,0) then dir .== 4 &&& color .== 9 &&& dist .== 0 &&& exactlyOne pointingAtMe
    else if coord == (1,0) then checkDir &&& color .== targColor &&& dist .== targDist + 1 &&& (bnot $ bOr pointingAtMe)
                      else  checkDir &&& color .== targColor &&& dist .== targDist + 1 
                            &&& exactlyOne pointingAtMe     
                    
  where ((lowB,_),(upB,_)) = bounds arr
        neighbours = catMaybes $ map (\dir -> (,dir) <$> (updateCoord upB coord dir)) [0..3]
        neighElems = map (\(coord,dir) -> (dir,arr!coord)) neighbours
        checkDir = bAny (.== dir) $ map snd neighbours
        (targDir, targColor, targDist) = matchDir neighElems dir
        pointingAtMe = map (\(neighCoord,neighDir) -> unDir (arr!neighCoord) .== oppDirection neighDir) neighbours 
                        

-- A source is not allowed to have anything going into it.
-- Every other square has exactly one path in.
-- A sink contains no arrow.

-- Generating neighbours with lists?
-- Basically zip above, below left and right with special cases for boundaries
-- and for sources and sinks. We could encode this as a tag in the tuple,
-- it might all just me easier though using arrays.

isMagic :: Board -> SBool
isMagic rows = (bAnd $ map (checkElement arr) (assocs arr)) 
  where items = rows
        arr = listArray ((0,0),(n-1,n-1)) $ concat rows
        n = length rows


-- | Group a list of elements in the sublists of length @i@
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = let (f, r) = splitAt i xs in f : chunk i r

test1 :: Symbolic [(SWord8,SWord8)]
test1 =  mapM (const ((,) <$> exists_ <*> exists_)) [1..10]

existsPairN n = mapM (const ((,,) <$> exists_ <*> exists_ <*> exists_)) [1..n]

-- | Given @n@, magic @n@ prints all solutions to the @nxn@ magic square problem
cover :: Int -> IO ()
cover n
 | n < 0 = putStrLn $ "n must be non-negative, received: " ++ show n
 | True  = do putStrLn $ "Finding all " ++ show n ++ "-magic squares.."
              res <- allSat $ (isMagic . chunk n) <$> existsPairN n2 
              cnt <- displayModels disp res
              putStrLn $ "Found: " ++ show cnt ++ " solution(s)."
   where n2 = n * n
         disp i (_, model)
          | lmod /= n2
          = error $ "Impossible! Backend solver returned " ++ show n ++ " values, was expecting: " ++ show lmod
          | True
          = do when (i > 10) $ error "...More"
               putStrLn $ "Solution #" ++ show i
               mapM_ printRow board
               putStrLn $ "Valid Check: " ++ show (isMagic sboard)
               putStrLn "Done."
          where lmod  = length model
                board = chunk n model
                sboard = map (map (\(x,y,z) -> (literal x, literal y,literal z))) board
                sh2 z = let s = show z in if length s < 2 then ' ':s else s
                printRow r = putStr "   " >> mapM_ (\x -> putStr (sh2 x ++ " ")) r >> putStrLn ""







