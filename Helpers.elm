{-
Helpers

helper functions without a dependency to the model so they can be
used by the model.
e.g. fst/snd
-}
module Helpers exposing (..)

import Set exposing (..)
import Dict exposing (..)
import Char exposing (fromCode)
import String exposing (fromChar)


{-
Takes a Maybe
Returns True if the Maybe is a Just, otherwise returns False
-}
isJust maybe =
    case maybe of
        Nothing -> False
        _ -> True


{-
Just add Just
-}
addJust : a -> Maybe a
addJust val = Just(val)


{-
Mappable player control keys
-}
playerCmdKeys = {
    p1Up = 'W'
    ,p1Down = 'D'
    ,p2Up = 'I'
    ,p2Down = 'J'
    }




{-
The sets of keys to ignore/include
-}
--excludedKeys = Set.fromList([ "\t", "Q" ])
includedKeys = Set.fromList([
    playerCmdKeys.p1Up
    , playerCmdKeys.p1Down
    , playerCmdKeys.p2Up
    , playerCmdKeys.p2Down
    ])


{-
Takes a key code
Returns a bool indicating if the key should be monitored/stored
-}
isKeyIncluded : Char -> Bool
isKeyIncluded key =
    Set.member key includedKeys




{-
Takes a key code representing the key pressed
Checks if the key is 'excluded'
If not, adds to the set of 'currently pressed keys'
-}
addKey : Char -> Set Char -> Set Char
addKey newKey keys =
    case isKeyIncluded newKey of
        True -> Set.insert newKey keys
        False -> keys -- it's excluded. return previous set



{-
Takes a key code
Removes that key from our set of currently pressed keys
-}
removeKey : Char -> Set Char -> Set Char
removeKey k keys = Set.remove k keys


{-
Takes a key code
Returns True if that key is stored in our "pressed keys" set
Returns False otherwise
-}
isKeyPressed : Char -> Set Char -> Bool
isKeyPressed key keys = Set.member key keys


{-
Takes a key code
Returns the string representation of the key
-}
key2String : Int -> String
key2String k = (fromChar <| fromCode <| k)


{-
Tuple access helpers (first/second)
-}
fst : (a, a) -> a
fst tuple = Tuple.first tuple

snd : (a, a) -> a
snd tuple = Tuple.second tuple



