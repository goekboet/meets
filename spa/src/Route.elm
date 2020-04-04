module Route exposing 
    ( Route(..)
    , toRoute
    , route
    , routeToUrl
    , logoutUrl
    , setFilterParameter
    )

import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query
import Url.Builder as UrlB
import Browser.Navigation exposing (Key, pushUrl)
import Model exposing (Msg)

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

routeToUrl : Route -> String
routeToUrl r =
    case r of
        ScheduleRoute s w ->
            UrlB.absolute [ "hosts", s ] (Maybe.map (\x -> [ UrlB.string "week" x ]) w |> Maybe.withDefault [])

        _ ->
            UrlB.absolute [] []

logoutUrl : Route -> String
logoutUrl r =
    UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (routeToUrl r) ]

setFilterParameter : Key -> Route -> String -> Cmd Msg 
setFilterParameter key r filter =
    case r of
        HostsRoute _ -> 
            let
                query = UrlB.string "notBeforeName" filter
                url = UrlB.absolute [ "hosts" ] [ query ]
            in
                pushUrl key url
            
        _           -> Cmd.none