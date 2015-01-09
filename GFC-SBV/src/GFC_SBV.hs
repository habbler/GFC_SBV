{-# LANGUAGE TupleSections #-}
module GFC_SBV where

import Data.Array
import Data.List (findIndex)
import Data.Maybe

import Data.SBV
--import Control.Applicative
import Control.Monad (when)
import Control.Applicative ((<$>),(<*>))

type Direction = SWord8

type Colour = SWord8
type Distance = SWord8

type Elem = (Direction, Colour, Distance)

unDir :: Elem -> Direction
unDir (dir,_,_) = dir

type Row = [Elem]

type Board = [Row]

type Coord = (Int, Int)

-- | The end-points of a trail
data EPTrail = EndPoints { epSource :: Coord
                         , epSink  :: Coord 
                         } deriving (Eq, Show)

-- | The problem setup. A particular size and sources and sinks
data Grid = Grid { gSize :: (Int,Int)
                 , gTrails :: [EPTrail] 
                 }

-- | Find the coord in direction d from give coord
updateCoord :: Int -> Coord -> Direction -> Maybe Coord
updateCoord uBound (x,y) direction = case direction of 
                                        0 -> cond (y < uBound) (x,y+1) -- N
                                        1 -> cond (x < uBound) (x+1,y) -- E
                                        2 -> cond (y > 0     ) (x,y-1) -- S  
                                        3 -> cond (x > 0     ) (x-1,y) -- W
     where cond c v = if c then Just v else Nothing

-- | The opposite direction.     
oppDirection :: Direction -> Direction
oppDirection dir = ite (dir .== 4)  4 $  -- No direction
                   let newDir = dir + 2 in
                   ite (newDir .> 3) (newDir - 4) newDir
        
-- | Return the matching element from the list, otherwise a dummy default value
matchDir :: [(Direction,Elem)] -> Direction -> Elem
matchDir [] _ = (4,255,255)
matchDir (x@(dir,elem):xs) direct = ite (direct .== dir) elem (matchDir xs direct)                                      

-- | Return true if exactly one value is true.       
exactlyOne :: [SBool] -> SBool
exactlyOne [] = false            
exactlyOne (t1:tRest) = t1 &&& bAnd (map bnot tRest) ||| (bnot t1 &&& exactlyOne tRest)                           

-- | Check one square of the grid. Generate the satisfying equation
checkElement :: Array Coord Elem -> Grid -> (Coord,Elem) -> SBool
checkElement arr puzzle (coord, (dir,color, dist)) = 
    case findSink of
      Just colorIdx -> dir .== 4 &&& color .== colorIdx &&& dist .== 0 &&& exactlyOne pointingAtMe
      _ -> case findSource of
             Just colorIdx -> checkDir &&& color .== colorIdx &&& color .== targColor &&& dist .== targDist + 1
                               &&& bnot (bOr pointingAtMe)
                  -- A normal square, not a sink or a source             
             _ -> checkDir &&& color .== targColor &&& dist .== targDist + 1 
                  &&& exactlyOne pointingAtMe        
                    
  where ((lowB,_),(upB,_)) = bounds arr
        -- coord and direction of neighbours of coord
        neighbours = mapMaybe (\dir -> (,dir) <$> updateCoord upB coord dir) [0..3]
        neighElems = map (\(coord,dir) -> (dir,arr!coord)) neighbours -- The values (Elems)
        -- Check that the direction is a valid one; it points to a neighbour
        checkDir = bAny (.== dir) $ map snd neighbours 
        -- Get the element of the next neighbour on the trail (pointed to by dir)
        (targDir, targColor, targDist) = matchDir neighElems dir
        -- The neighbours that are pointing in my direction. i.e. they flow into coord
        pointingAtMe = map (\(neighCoord,neighDir) -> unDir (arr!neighCoord) .== oppDirection neighDir) neighbours
        -- Is the current coord a source. Use the index as color
        findSource = fromIntegral <$> findIndex (\(EndPoints source _) -> source == coord) trails
        -- Is the current source a sink
        findSink = fromIntegral <$> findIndex (\(EndPoints _ sink) -> sink == coord) trails
        trails = gTrails puzzle
                        
findCover :: Grid -> [Elem] ->  SBool
findCover  puzzle@(Grid (rows, cols) _) vars = bAnd $ map (checkElement arr puzzle) (assocs arr) 
  where arr = listArray ((0,0),(rows-1,cols-1)) vars


-- | Group a list of elements in the sublists of length @i@
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = let (f, r) = splitAt i xs in f : chunk i r

existsPairN :: (SymWord a1, SymWord a2, SymWord a3) => Int -> Symbolic [(SBV a1, SBV a2, SBV a3)]
existsPairN n = mapM (const ((,,) <$> exists_ <*> exists_ <*> exists_)) [1..n]

-- | Given @puzzle@, cover @puzzle@ finds a perfect cover of trails as specified by puzzle
cover :: Grid -> IO ()
cover puzzle@(Grid (rows, cols) _)  
         = do putStrLn "Finding all covers."
              res <- allSat $ findCover puzzle <$> existsPairN n2 
              cnt <- displayModels disp res
              putStrLn $ "Found: " ++ show cnt ++ " solution(s)."
   where n2 = rows * cols
         disp i (_, model)
          | lmod /= n2
          = error $ "Impossible! Backend solver returned " ++ show n2 ++ " values, was expecting: " ++ show lmod
          | True
          = do when (i > 10) $ error "...More"
               putStrLn $ "Solution #" ++ show i
               mapM_ printRow board
               putStrLn $ "Valid Check: " ++ show (findCover puzzle sboard)
               putStrLn "Done."
          where sboard = map (\(x,y,z) -> (literal x, literal y,literal z)) model
                lmod  = length model
                board = chunk cols model
                sh2 z = take 10 $ " "++(show z)++"           "
                printRow r = putStr "   " >> mapM_ (\x -> putStr (sh2 x)) r >> putStrLn ""

puzzle1 :: Grid
puzzle1 = Grid (9,9) 
         [EndPoints (3,5) (6,5), EndPoints (1,2) (5,1), EndPoints (7,1) (5,3)
         ,EndPoints (2,2) (6,4), EndPoints (3,2) (6,6),  EndPoints (5,4) (7,0)
         ,EndPoints (5,0) (8,0)
         ]

test1 :: IO ()
test1 = cover puzzle1




