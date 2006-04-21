{-# OPTIONS -funbox-strict-fields #-}
-- | The Parsec parser needs to keep track of various state
-- information while matching a regular expression.  This includes the
-- number of accepted characters and the in-progross and completed
-- substring matches.  This module defines that state and some
-- convenience functions.
--
-- A user defined state can be maintained via newState, getUserState,
-- setUserState, and updateUserState.
--
-- The 'FullState' type is opaque, allowing for better abstraction.
-- Note that calling stopSub when startSub has not been called will
-- trigger an 'error', and that initState/finalState call startSub
-- 0/stopSub 0 respectively.
module Text.Regex.Lazy.RegexParsecState 
    (initState,finalState,newState
    ,getUserState,setUserState,updateUserState
    ,incState,plusState
    ,startSub,stopSub
    ,lookupSub,lookupSubs,lookupAccepted) where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Text.Regex.Lazy.Common
import Text.ParserCombinators.Parsec
import qualified Data.IntMap as I
import Data.IntMap(IntMap)
import Control.Monad(liftM)

-- | 'initState' sets the accepted characters to zero, forgets all
-- substring matching (in-progress or complete) and starts sub 0 (the
-- whole match).  This operation preserves the user state.
initState = do -- keep only userState
               updateState (\state -> newState (userState state))
               startSub 0

-- | 'finalState' stops sub 0 (the whole match) and returns an IntMap
-- of all the completed substring matches.  Any in-progress matches
-- are ignored.
finalState = do stopSub 0
                liftM closedSub getState

-- | This takes a value of the user state and returns the full parsec
-- state with zero accepted characters and no in-progress or completed
-- substring matches.
newState user = FullState {userState = user
                          ,accepted = 0
                          ,openSub = I.empty
                          ,closedSub = I.empty}

-- | This returns the value of the user state
getUserState = liftM userState getState

-- | The replaces the user state with the provided value
setUserState user = do state <- getState
                       setState $ state {userState = user}

-- | This applies the given function to the current value of the user state
updateUserState f = updateState (\state@FullState{userState=user}->state{userState=f user})

-- | This adds one to the number of accepted characters
incState = updateState (\state@(FullState {accepted=pos})->state {accepted=succ pos})

-- | This adds the provided number to the count of accepted characters
plusState n = updateState (\state@(FullState {accepted=pos})->state {accepted=n + pos})

-- | Called with PatternIndex i, this makes the i'th substring match
-- inprogress and marks the start of the i'th substring match at the
-- current position.  This does affect any value for the completed
-- i'th substring match.
startSub i = do 
  state@(FullState {accepted=n,openSub=open}) <- getState
  here <- getInput
  let open' = I.insert i (here,n) open
      state' = state {openSub = open'}
  setState state'

-- | Called with PatternIndex i, this captures the input from point
-- where startSub i was called until the current location.  It removes
-- the in-progress i'th substring and sets the i'th completed
-- substring to the new value, overwriting any previous value.
--
-- The completed value is available from lookupSub i or in the map
-- provided by lookupSubs.
--
-- If the i'th substring is not open when this is called then stopSub
-- will call 'error'.
stopSub i = do
  state@(FullState {accepted=n,openSub=open,closedSub=closed}) <- getState
  let (mEntry,open') = I.updateLookupWithKey del i open
      del _ _ = Nothing
  case mEntry of
    Nothing -> error ("RegexParsecState: Could not closeSub "++show i)
    Just (here,pos) -> let sub = take (n-pos) here
                           closed' = I.insert i sub closed
                           state' = state {openSub=open',closedSub=closed'}
                       in setState state'

-- | 'lookupSub' i returns Just the completed captured between 'startSub'
-- i and 'stopSub' i or Nothing if there has been no capture.
lookupSub i = liftM ((I.lookup i).closedSub) getState

-- | 'lookupSubs' returns an IntMap of all the completed substring captures.
lookupSubs = liftM closedSub getState

-- | 'lookupAccepted' return the number of accepted input characters.
-- This is typicaly counted from the call to initState.
lookupAccepted = liftM accepted getState
