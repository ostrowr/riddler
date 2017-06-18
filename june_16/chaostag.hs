import qualified Math.Combinat.Partitions as Partitions
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.MultiSet as MultiSet
import qualified Data.Matrix as Matrix
import Data.Either.Unwrap
import Data.Ratio

-- given a state and an action ((tagger, numPeopleTaggerHasTagged), (taggee, num...))
-- return a new state reflecting that the action (tag) has been made.
tag :: [Int] -> ((Int, Int), (Int, Int)) -> [Int]
tag state action = let (taggerIndex, taggerCnt) = fst action
                       (taggeeIndex, taggeeCnt) = snd action
                       newlyUntagged = take taggeeCnt $ repeat 0
                       newTaggerValue = taggerCnt + 1
                       newTaggeeValue = -1
                       newPartialState = [a | (i, a) <- (zip [0..] state),
                                              i /= taggerIndex,
                                              i /= taggeeIndex,
                                              i >= taggeeCnt]
                   in List.sort (newTaggeeValue:newTaggerValue:newPartialState ++ newlyUntagged)

-- find all transitions and their probabilities from a single state.
transitions :: [Int] -> Map.Map [Int] (Ratio Integer)
transitions state = let nPlayers = length state
                        stateWithIndices = zip [0..] state
                        actions = [((ia, a), (ib, b)) | (ia, a)<-stateWithIndices,
                                                        (ib, b)<-stateWithIndices,
                                                        ia /= ib,
                                                        a /= -1,
                                                        b /= -1]
                        ts = map (tag state) actions
                        nTransitions = toInteger $ length ts
                        probabilities = Map.map (\x -> (toInteger x) % nTransitions) (MultiSet.toMap (MultiSet.fromList ts))
                        singleton = Map.singleton state 1
                    in if (last state) == (nPlayers - 1) then singleton else probabilities

-- list all states in an n-player game.
allStates :: Int -> [[Int]]
allStates n = let allPartitions = foldl (++) [] (map Partitions._partitions [0..n-1])
                  expandPartition partition = let s = sum partition
                                                  l = length partition
                                                  zeroes = take (n - s - l) $ repeat 0
                                                  taken = take s $ repeat (-1)
                                              in List.sort $ partition ++ zeroes ++ taken
              in reverse $ List.sort $ filter (\x -> length x == n) $ map expandPartition allPartitions

-- list all transition probabilities for all states
-- in an n-player game.
transitionMap :: Int -> [Map.Map [Int] (Ratio Integer)]
transitionMap n = map transitions $ allStates n

-- The transition matrix for an n-player game.
transitionMatrix :: Int -> Matrix.Matrix (Ratio Integer)
transitionMatrix n = let stateList = allStates n
                         transitionMap = map transitions stateList
                         nStates = length stateList
                         value (i, j) = let tMap = transitionMap !! (i - 1)
                                            key = stateList !! (j - 1)
                                        in Map.findWithDefault 0 key tMap
                     in Matrix.matrix nStates nStates value

-- the fundamental matrix is the expected number of visits to a transient state j
-- starting from a transient state i (before being absorbed).
-- The probability of transitioning from i to j in exactly k steps is the (i,j)-entry of Qk.
-- Summing this for all k (from 0 to ∞) yields the fundamental matrix, which can be
-- directly calculated as (I - Q)^-1 where Q is the submatrix of non-absorbing states.
-- see https://en.wikipedia.org/wiki/Absorbing_Markov_chain
fundamentalMatrix :: Int -> Matrix.Matrix (Ratio Integer)
fundamentalMatrix n = let trs = transitionMatrix n
                          q_size = Matrix.nrows trs - 1
                          q = Matrix.submatrix 1 q_size 1 q_size trs
                          i = Matrix.identity q_size
                          fundamental = Matrix.inverse (i - q)
                      in fromRight fundamental

-- The expected number of steps before being absorbed.
expectedSteps :: Int -> (Ratio Integer)
expectedSteps n = let fundamental = fundamentalMatrix n
                      colVector = Matrix.matrix (Matrix.nrows fundamental) 1 (\_ -> 1)
                      t = fundamental * colVector
                   in t Matrix.! (1,1)

-- turns out that this just seems to be 2^(N-1) - 1
main = mapM_ print $ zip [2..] (map expectedSteps [2..])