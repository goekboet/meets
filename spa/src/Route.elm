module Route exposing (Route(..), toRoute, route)

import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query

type alias WeekParameter = String
type alias HostHandle = String
type alias NameFilterParameter = String


type Route
    = NotFound
    | HomeRoute (Maybe WeekParameter)
    | HostsRoute (Maybe NameFilterParameter) 
    | ScheduleRoute HostHandle (Maybe WeekParameter)
    | BookingsRoute


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound <| UrlP.parse route url


route : Parser (Route -> a) a
route =
    UrlP.oneOf
        [ UrlP.map HomeRoute (UrlP.top <?> Query.string "week")
        , UrlP.map HostsRoute (UrlP.s "hosts" <?> Query.string "notBeforeName" )
        , UrlP.map ScheduleRoute (UrlP.s "hosts" </> UrlP.string <?> Query.string "week")
        , UrlP.map BookingsRoute (UrlP.s "bookings")
        ]