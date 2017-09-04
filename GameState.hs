{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module GameState where

{-
    
    `s` is the states type, and `a` is actions type.
-}
class GameState s a | s -> a where
    {-
	Return the number of the player who is going to make the current move.
    -}
    playerIndex :: s -> Int

    {-
        Returns the number of players
    -}
    maxPlayers :: s -> Int

    {-
	For the given state, return the list of the successors state, each state
	being a tuple with the action that led to it.
    -}

    successors :: s -> [(a, s)]
    {-
        Returns the meaning of the current state.
    -}
    outcome :: s -> Outcome

{-
    Game types.
-}
data Outcome
    = Win  Float  -- One player won, with the given score.
    | Draw Float  -- Game ended draw, with the given score.
    | Ongoing     -- The game has not finished.
    deriving (Eq, Show)
