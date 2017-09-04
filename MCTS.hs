
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-} --for Eq

module MCTS where

import GameState

import Prelude hiding (traverse)
import System.Random
import Data.List
import Data.Function
import Debug.Trace
{-
    *** TODO ***

    Implement the type `Tree s a`, for search trees, where `s` represents
    the type of the states, and `a`, the type of the actions.

    The following attributes are needed:
    * current state
    * the action that lead to the current state
    * number of visits
    * score
    * children.=
-}

data Tree s a = Tree{
  score  :: Float,
  visits :: Int,
  state  :: s,
  action :: a,
  childs :: [Tree s a]
}

{-
    *** TODO ***

    Implement the type `Zipper s a`, for traversing the search tree of type
    `Tree s a`, where `s` represents the state type, and `a` the action
    type.

    Among the specific components of a zipper, there will also be a attriute for
    a random number generator, modified among the tree exploration.
-}

data Zipper s a = Zipper {
  visit  :: [Tree s a],
  node    :: Tree s a,
  gen    :: StdGen,
  status  :: s,
  act    :: a
}

{-
    *** TODO ***

    Instantiate the class `Show` with type `Tree s a`.
-}
instance (Show s, Show a) => Show (Tree s a) where
    show  t =  "score :" ++ show (score t) ++ "visits:" ++ show(visits t) ++ "state:" ++ show(state t) ++ "\n" ++ intercalate "|" (map show (childs t))

{-
    ****************
    Access functions
    ****************
-}

{-
    *** TODO ***

    Return the state of a node.
-}
treeState :: Tree s a -> s
treeState t = (state t)

{-
    *** TODO ***

    Return the action of a node.
-}
treeAction :: Tree s a -> a
treeAction t = (action t)

{-
    *** TODO ***

    Return the score of a node.
-}
treeScore :: Tree s a -> Float
treeScore t = (score t)

{-
    *** TODO ***

    Return the number of visitors of a node.
-}
treeVisits :: Tree s a -> Int
treeVisits t = (visits t)

{-
    *** TODO ***

    Return the children of a node.
-}
treeChildren :: Tree s a -> [Tree s a]
treeChildren t = (childs t)

{-
    *** TODO ***

    Return the node centered in a zipper.
-}

zipperTree :: Zipper s a -> Tree s a
zipperTree z = (node z)

{-
    *** TODO ***

    Return the random rumber generator from a zipper.
-}
zipperGen :: Zipper s a -> StdGen
zipperGen z = (gen z)

{-
    *****************
    Funcții pe arbori
    *****************
-}

{-
    *** TODO ***

    Build a search tree (eventually infinite), starting fomr the generating
    function of the successors of a state and the initial state.
-}
helper :: (s -> [(a, s)])
        -> a
        -> s
        ->Tree s a

helper generator actiune status = Tree 0 0  status actiune (map (\x -> helper generator (fst x) (snd x)) (generator status))

expand :: (s -> [(a, s)])  -- Generator of succeeding states
       -> s                -- initial state
       -> Tree s a         -- Search tree
expand generator status = Tree 0 0 status undefined (map (\x -> helper generator (fst x) (snd x)) (generator status))

{-
    *** TODO ***

    Explore the tree, choosing at every step a random successor,
    until a finish state is attained (win/ draw).

    Întoarce:
    * the inverted path, such as the first element of the path is the terminal
      node
    * meaning of the terminal state (win/ draw)
    * the final state of number generator
-}

rolloutTreehelper :: GameState s a =>
               Tree s a ->
               StdGen ->
               [Tree s a] ->
               ([Tree s a], Outcome, StdGen)

rolloutTreehelper t gen lista =
  if (outcome (state t) /= Ongoing) then
    (t : lista, (outcome (state t)), gen)
  else
    (rolloutTreehelper ( (childs t) !! ( (fst (next gen)) `mod` (length (childs t))) ) (snd (next (snd(next gen)))) (t : lista))
    where
      g = (next gen)

rolloutTree :: GameState s a =>
               Tree s a ->
               StdGen ->
               ([Tree s a], Outcome, StdGen)

rolloutTree t gen = rolloutTreehelper t gen []


{-
    *** TODO ***

    Find the best child of a node (the one with best score/number of visits ratio)

    Hint: `maximumBy` și `comparing`.
-}

compareHelper :: Tree s a ->
                 Float
compareHelper t = (score t) / (fromIntegral (visits t))

compareElements :: [Tree s a] -> Tree s a
compareElements t = maximumBy (compare `on` compareHelper) t
bestChild :: Tree s a -> Tree s a
bestChild t = bestChildHelper (childs t) 0
bestChildHelper :: [Tree s a] -> Int -> Tree s a
bestChildHelper t it = if (it == (length t)) then (t !! 0)
                       else if (visits(t !! it) == 0) then (t !! it)
                       else (bestChildHelper t (it + 1))

{-
    *******************
    Zipper functions
    *******************
-}

{-
    *** TODO ***

    Create a zipper centered on a given tree, that uses the random number
    generator given as parameter.
-}
getZipper :: Tree s a -> StdGen -> Zipper s a
getZipper t gen = Zipper [] t gen (state t) (action t)

{-
    *** TODO ***

  Check if the zipper is centered in the root of the tree.
-}
isRoot :: Zipper s a -> Bool
isRoot z = (null (visit z))

{-
    *** TODO ***

    The ucb1 value from the youtube video (constant C = 2).
-}
ucb1 :: Float  -- chid score
     -> Int    -- child visitors
     -> Int    -- parent visitors
     -> Float  -- estimation

ucb1 score childVisit parentVisit = score / (fromIntegral childVisit) +
      2 * (sqrt
      ((log (fromIntegral parentVisit)) / (fromIntegral childVisit)))

{-
    *** TODO ***
    For the node with a centered zipper, select the child with the maximum
    ucb1 value. Return the zipper centerd on that specific child.

    An unvisited node has the ucb1 value infinite and therefire will be always
    chosen.
-}


getIndex :: Eq s =>
               [Tree s a]
            -> Tree s a
            -> Int
            -> Int
getIndex lista el idx = if ( (state (lista !! idx)) ==  (state el)) then idx
                        else (getIndex lista el (idx + 1))


listWithoutNode :: Eq s =>
              [Tree s a]
           -> Tree s a
           -> [Tree s a]
listWithoutNode l nod = (fst newlist) ++ (drop 1 (snd newlist)) where
                    newlist = (splitAt (getIndex  l nod 0) l)

selectucb :: Tree s a
         -> Tree s a
         -> Float

selectucb parent child = (score child) / (fromIntegral (visits child)) +
      2 * (sqrt
      ((log (fromIntegral (visits parent))) / (fromIntegral (visits child))))

selectHelper :: Zipper s a
             -> Tree s a

selectHelper z = maximumBy (compare `on` selectucb (node z)) (childs (node z))

select :: Eq s => Zipper s a -> Zipper s a
select z = if ((null (childs (node z)))) then
                Zipper (visit z) seed (gen z) (status z) (act z)
          else  Zipper ((Tree (score (node z)) (visits (node z)) (state (node z)) (action (node z))  (listWithoutNode (childs (node z)) seed ))
                :(visit z)) seed (gen z) (status z) (act z)
          where
                seed = (selectHelper z)
{-
    *** TODO ***

  Apply `select` until an unvisited or leaf node is found.
-}

traverse :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
traverse  z = if ( (visits (node z)) == 0 || (null (childs (node z)))) then
                Zipper (visit z) (node z) (snd (next (gen z))) (status z) (act z)
              else (traverse (select z))

{-
    *** TODO ***

    Apply `rolloutTree` for the tree with the centered zipper.

    Return:
    * the new score for the nodes on the path that lead to finding the leaf node
    * the player that did the move
    * the new zipper, with the random number generator returned by
      `rolloutTree`.

    For the task with minimum two players, the score and the player's number
    will be calculated:
    * For victory, it is used the score from the oject `Outcome` and the number
      of the player found in the terminal state.
    * For draw, it is used the score from the object `Outcome` devided by the
      number of players, and `Nothing`.
-}
draw ::Outcome -> Bool
draw (Draw _) = True
draw  _ = False

win ::Outcome -> Bool
win( Win _) = True
win _ = False

scor ::Outcome
     ->Float
scor (Win  result) = result
scor (Draw result) = result
scor _ = 0


getScore ::  GameState s a => Tree s a
                       -> Outcome
                       -> (Float, Maybe Int)
getScore t  out = if ((draw out) == True ) then ( (scor out) / (fromIntegral (maxPlayers (state t))), Nothing)
                        else ((scor out), (Just (playerIndex (state t))))

rolloutZipper :: GameState s a => Zipper s a -> (Float, Maybe Int, Zipper s a)
rolloutZipper z = let (lista, rez, nou_generator)  = (rolloutTree (node z) (gen z))
                      valori = (getScore (head lista) rez)
                  in
                    ((fst valori), (snd valori),
                    Zipper (visit z) (node z) nou_generator (status z) (act z))


{-
    *** TODO ***

    Climb a level in tree.
-}
toParent :: Zipper s a -> Zipper s a
toParent z =  Zipper (drop 1 (visit z)) (newnode) (gen z) (state newnode) (action newnode)
                  where newnode = (head (visit z))

{-
*** TODO ***

    Implement the backpropagation step, there the three components are then
    parameters returned by `rolloutZipper`.

    Therefore, the path to root is followed and the number of visits for all
    the nodes founded in the path is incremented by 1.
    The score is modifed accordingly:
    * In case of winning only the nodes that belong to the player that won are
      updated.
    * In case of draw, all the nodes are updated.

    Zipper-ul final este centrat pe rădăcină.
-}


getStuff :: Maybe Int -> Int
getStuff(Just p) = p
getStuff Nothing = 999

updateNode :: GameState s a =>
              Tree s a
           -> Int
           -> Maybe Int
           -> Float
           -> Tree s a

updateNode nod ite pl scor = if((getStuff (pl)) == 999 || (getStuff (pl)) == (playerIndex( state nod))) then
                            Tree (scor + (score nod)) (ite) (state nod) (action nod) (childs nod)
                         else
                            Tree (score nod) (ite) (state nod) (action nod) (childs nod)

backPropHelper :: GameState s a =>
                  Float
               -> Maybe Int
               -> [Tree s a]
               -> Zipper s a
               -> Int
               -> Zipper s a
backPropHelper scor player t z ite = if (null(visit z)) then
                                      Zipper []
                                        (updateNode (Tree (score (node z)) (ite) (state (node z)) (action (node z)) ((childs (node z)) ++ t) ) (visits (node z) + 1) player scor)
                                        (gen z) (status z) (act z)
                                     else
                                       (backPropHelper scor player [(updateNode (node z) ite player scor)]
                                        (Zipper (drop 1 (visit z)) (Tree (score (newnode )) (visits(newnode)) (state newnode) (action newnode) ((childs newnode) ++ t))
                                        (gen z) (state newnode) (action newnode)) (ite + 1))
                                     where newnode = (head (visit z))

backProp :: GameState s a => Float -> Maybe Int -> Zipper s a -> Zipper s a

backProp score player z = backPropHelper score player [] z 1

{-
    *** TODO ***

    One full iteration with all the steps, starting from a random node and
    finishing at the root.
-}

exploreOne :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
exploreOne z = let trec = (traverse z)
                   score_pl = (getScore (node trec) (outcome (state (node trec)))) in
               (backProp (fst score_pl) (snd score_pl) trec)

{-
    *** TODO ***

    Execute a given number of iterations.
-}
compareValue ::  GameState s a =>
                 Zipper s a
              -> Zipper s a
              -> Zipper s a

compareValue z1 z2 = if( scor(outcome(state(node z1))) > scor(outcome(state(node z2)))) then
                        z1
                     else z2

exploreManyHelper ::(Eq s, GameState s a) =>
                     Int
                  -> Zipper s a
                  -> Zipper s a
                  -> Zipper s a
exploreManyHelper number z best = if(number == 0) then
                            (compareValue best z)
                       else (exploreManyHelper (number - 1) (exploreOne z) (compareValue z best))


exploreMany :: (Eq s, GameState s a) => Int -> Zipper s a -> Zipper s a
exploreMany number z = exploreManyHelper number z z
{-
    *** TODO ***

    Choose an action starting from a status and a desired number of iterations.
    Return a pair with the action and the status returned.

    The function should check if one of the following states is a victory,
    in which case it is chosen directly. Otherwise the search process is
    continued.

    After all the iterations completed, the action is simply chosen using
    `bestChild`.

-}

chooseHelper ::(Eq s, GameState s a) => Int -> s -> StdGen -> Zipper s a
chooseHelper ite state gen =
                            (exploreMany ite (select z)) where
                            z = (getZipper (expand successors state) gen)

choose :: (Eq s, GameState s a) => Int -> s -> StdGen -> (a, s)
choose it status gen = let z = (chooseHelper it status gen) in
                      ( (action (bestChild (node z))), (state (bestChild(node z))))
