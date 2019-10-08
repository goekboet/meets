module Week exposing 
    ( WeekName
    , s2WeekOrder
    , nextWeek
    , lastWeek
    , posix2Week)

import Time as T exposing (Posix, Zone, Month(..), Weekday)
import Set as S exposing (Set)

type alias WeekName = (String, String)

longYears : Set Int
longYears = S.fromList [
    2004,2009,2015, 2020,2026,
    2032,2037,2043, 2048,2054,
    2060,2065,2071, 2076,2082,
    2088,2093,2099, 2105,2111,
    2116,2122,2128, 2133,2139,
    2144,2150,2156, 2161,2167,
    2172,2178,2184, 2189,2195,
    2201,2207,2212, 2218,2224,
    2229,2235,2240, 2246,2252,
    2257,2263,2268, 2274,2280,
    2285,2291,2296, 2303,2308,
    2314,2320,2325, 2331,2336,
    2342,2348,2353, 2359,2364,
    2370,2376,2381, 2387,2392,
    2398
    ]

nextWeek : (Int, Int) -> (Int, Int)
nextWeek (y, w) =
    let
        last = if (S.member y longYears) then 53 else 52
    in
        if w == last then (y + 1, 1) else (y, w + 1)

lastWeek : (Int, Int) -> (Int, Int)
lastWeek (y, w) =
    let
        last = if (S.member (y - 1) longYears) then 53 else 52
    in
        if w == 1 then (y - 1, last) else (y, w - 1)


withinWeekBounds : (Int, Int) -> Maybe (Int, Int)
withinWeekBounds (y, w) =
    if S.member y longYears
    then if w > 0 && w <= 53 then Just (y, w) else Nothing
    else if w > 0 && w <= 52 then Just (y, w) else Nothing

s2WeekOrder : String -> Maybe (Int, Int)
s2WeekOrder s = case s2Week s of
            Just (a, b) -> Maybe.map2 Tuple.pair (String.toInt a) (String.toInt b)
            _ -> Nothing
    
s2Time : Zone -> String -> Maybe (Posix, Posix)
s2Time tz s =
    case s2WeekOrder s of
        Just (y, w) -> Nothing
        _           -> Nothing

s2Week : String -> Maybe WeekName
s2Week s = 
    let
        assertLength n x = if String.length x == n then Just x else Nothing
        p n = (Maybe.andThen String.toInt) << assertLength n
        toWeekname (x, y) = (String.fromInt x, String.padLeft 2 '0' <| String.fromInt y)
    in
        case String.split "-" s of
            [y, w] -> 
                Maybe.map2 Tuple.pair (p 4 y) (p 2 w) 
                    |> Maybe.andThen withinWeekBounds
                    |> Maybe.map toWeekname
            _ -> Nothing

posix2Week : Zone -> Posix -> (Int, Int)
posix2Week z t =
    let
        y = getWeekYear t z
        w = getWeekNumber t z
    in
        (y, w)
    
type alias Year = Int
type alias WeekNumber = Int

getWeekNumber : Posix -> Zone -> WeekNumber
getWeekNumber t z = 
    let
        nextThu = toNextThursday t z    
    in
        if List.any (\x -> x == (T.toWeekday z t)) [ T.Mon, T.Tue, T.Wed ]
        then thisYearsElapsed7d (T.toYear z nextThu) nextThu z
        else thisYearsElapsed7d (T.toYear z t) t z

getWeekYear : Posix -> Zone -> Year
getWeekYear t z = T.toYear z (toNextThursday t z)

-- helpers

h2ms t = t * 60 * 60 * 1000

toNextThursday : Posix -> Zone -> Posix
toNextThursday t z = 
    if T.toWeekday z t == T.Thu
    then t
    else toNextThursday (fwd24h t) z

thisYearsElapsed7d : Year -> Posix -> Zone -> Int 
thisYearsElapsed7d y t z = 
    if T.toYear z t == y
    then 1 + thisYearsElapsed7d y (back24h 7 t) z
    else 0

back24h : Int -> Posix -> Posix
back24h n t =
    let
        ms = T.posixToMillis t
        hs = n * h2ms 24
    in
        T.millisToPosix (ms - hs)

fwd24h : Posix -> Posix
fwd24h = back24h (-1) 