port module Weekpointer exposing 
    ( Weekpointer
    , Week
    , getWeekWindow
    , moveWeekpointer
    , newWeekpointer
    )

type alias Week = 
    { name : String
    , ts: Int
    , isNow : Bool
    }

type alias Weekpointer =
    { current: Week
    , previous: Week
    , next : Week
    }

getWeekWindow : Weekpointer -> (Int, Int)
getWeekWindow { current, previous, next } =
    (current.ts, next.ts)

port moveWeekpointer : Maybe Int -> Cmd msg
port newWeekpointer : (Weekpointer -> msg) -> Sub msg