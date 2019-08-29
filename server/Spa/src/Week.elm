module Week exposing 
    ( fromPosix
    , toMidnight
    , week
    , untilNextMidnight
    , mockStockholm)

import Time as T exposing (Posix, Zone, Month(..), Weekday)

type alias Week = List (Window, WeekDayName, GregorianDayName)

type alias Window = (Posix, Int)
type WeekDayName = ISO8601 Year WeekNumber Weekday    
type GregorianDayName = Gregorian Year Month DateNumber 

fromPosix : Zone -> Posix -> Week 
fromPosix z = week z << bckToMonday z

type alias Midnight = Posix

toMidnight : Posix -> Zone -> Midnight
toMidnight t z = 
    let
        h  = h2ms (T.toHour z t)
        m  = m2ms (T.toMinute z t)
        s  = s2ms (T.toSecond z t)
        ms = T.toMillis z t  
    in
        T.millisToPosix (T.posixToMillis t - h - m - s - ms)

week : Zone -> Midnight -> Week 
week z m =
    let
        wd = T.toWeekday z m
    in
        if wd == T.Sun
        then untilNextMidnight m z :: []
        else untilNextMidnight m z :: week z (fwdToMidnight wd m z)

untilNextMidnight : Midnight -> Zone -> (Window, WeekDayName, GregorianDayName)
untilNextMidnight m z =
    let
        w = dayWindow m z
        wd = getWeekDayname m z
        gd = getGregorianDayName m z
    in
        (w, wd, gd)
    
type alias Year = Int
type alias WeekNumber = Int
type alias DateNumber = Int

getWeekDayname : Midnight -> Zone -> WeekDayName
getWeekDayname t z = 
    let 
        year = getWeekYear t z
        w = getWeekNumber t z
        day  = T.toWeekday z t
    in
        ISO8601 year w day

getGregorianDayName : Posix -> Zone -> GregorianDayName
getGregorianDayName t z =
    let
        y = T.toYear z t
        m = T.toMonth z t
        d = T.toDay z t
    in
        Gregorian y m d

dayWindow : Midnight -> Zone -> Window
dayWindow m z =
    let
        t = T.posixToMillis m
        wd = T.toWeekday z m
        next = T.posixToMillis <| fwdToMidnight wd m z
    in
        (m, next - t)

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
m2ms t = t * 60 * 1000
s2ms t = t * 1000

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

offsetnH : Int -> Posix -> Posix
offsetnH n t =
    let
        ms = T.posixToMillis t
        offset = h2ms n
    in
        T.millisToPosix (ms + offset)

fwdToMidnight : Weekday -> Midnight -> Zone -> Midnight
fwdToMidnight refday m z = 
    if (T.toWeekday z m) == refday 
    then fwdToMidnight refday (offsetnH 13 m) z
    else toMidnight m z

bckToMonday : Zone -> Posix -> Midnight
bckToMonday z t =
    if (T.toWeekday z t == T.Mon)
    then toMidnight t z
    else bckToMonday z (offsetnH -13 t) 

-- Test helpers

dstStart2019 = 1553994000000
dstEnd2019 = 1572141600000

mockStockholm : Zone
mockStockholm = T.customZone 0 
    [ {start = dstEnd2019 // 60000, offset = 1 * 60}
    , {start = dstStart2019 // 60000, offset = 2 * 60} 
    ]