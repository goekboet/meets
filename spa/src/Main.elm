port module Main exposing (main)

import Apicall exposing (..)
import Browser
import Browser.Navigation as Nav
import Debounce as Debounce exposing (Debounce)
import Dict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Encode as Encode exposing (Value)
import Model exposing (..)
import Route exposing (..)
import Url
import Url.Builder as UrlB
import SessionState as Session exposing (SessionState, sessionstateView)
import HostsListing as Hosts exposing (HostsData, receiveData)



-- Appointment


port gotTimesClock : (Value -> msg) -> Sub msg


port gotBookingsClock : (Value -> msg) -> Sub msg


port getTimesClock : Value -> Cmd a


port getBookingsClock : Value -> Cmd a



routeHostId : Route -> Maybe HostId
routeHostId r =
    case r of
        ScheduleRoute id _ ->
            Just id

        _ ->
            Nothing



-- week-pointer


port getWeekpointer : Value -> Cmd a


encodeWeek : Week -> Value
encodeWeek w =
    Encode.string w


port gotWeekpointer : (Value -> msg) -> Sub msg


decodeTimesWindow : Decoder Window
decodeTimesWindow =
    Decode.map2 Tuple.pair (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)


decodeWeekpointer : Decoder WeekPointer
decodeWeekpointer =
    Decode.map4
        WeekPointer
        (Decode.field "prev" Decode.string)
        (Decode.field "curr" Decode.string)
        (Decode.field "next" Decode.string)
        (Decode.field "window" decodeTimesWindow)



-- type alias BookCommandRecord = ( UnixTs, ApiCall Appointment )
-- type alias UnbookCommandRecord = ( UnixTs, ApiCall () )


type alias BookCommandHistory =
    Dict UnixTs (ApiCall Appointment)


type alias UnbookCommandHistory =
    Dict UnixTs (ApiCall ())






type alias Model =
    { key : Nav.Key
    , csrf : String
    , route : Route
    , sessionState : SessionState
    , focus : WeekPointer
    , lastwptr : Debounce Window
    , apiBaseUrl : ApiBaseUrl
    , hostsdata : HostsData
    , timesCall : ApiCall (List Appointment)
    , bookingsCall : ApiCall (List Appointment)
    , bookCallHistory : BookCommandHistory
    , unbookCallHistory : UnbookCommandHistory
    }


type alias Flags =
    { name : Maybe String
    , csrf : String
    , focus : WeekPointer
    , apiBaseUrl : ApiBaseUrl
    }



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- Init


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        r =
            toRoute url

        hostId =
            routeHostId r
    in
    ( { key = key
      , csrf = flags.csrf
      , route = toRoute url
      , sessionState = Session.init flags.name
      , focus = flags.focus
      , lastwptr = Debounce.init
      , apiBaseUrl = flags.apiBaseUrl
      , hostsdata = { calls = [ Pending ], nextPage = 0, filter = "" }
      , timesCall =
            Maybe.map (always Pending) hostId
                |> Maybe.withDefault Uncalled
      , bookingsCall = Uncalled
      , bookCallHistory = Dict.empty
      , unbookCallHistory = Dict.empty
      }
    , Cmd.batch
        [ Maybe.map (always callBookings) flags.name
            |> Maybe.withDefault Cmd.none
        , Maybe.map (callTimes flags.apiBaseUrl flags.focus.window) hostId
            |> Maybe.withDefault Cmd.none
        , loadHosts Nothing r flags.apiBaseUrl
        ]
    )



-- UPDATE


hostSwitch : Url.Url -> Model -> Maybe HostId
hostSwitch url model =
    case ( toRoute url, model.route ) of
        ( ScheduleRoute nH _, ScheduleRoute oH _ ) ->
            if nH /= oH then
                Just nH

            else
                Nothing

        ( ScheduleRoute nH _, _ ) ->
            Just nH

        ( _, ScheduleRoute _ _ ) ->
            Nothing

        _ ->
            Nothing


loadHosts : (Maybe Int) -> Route -> ApiBaseUrl -> Cmd Msg
loadHosts p r base =
    case r of
        HostsRoute filter ->
            callHosts base filter p

        _ ->
            Cmd.none


weekpointerStaleness : Route -> WeekPointer -> Maybe Week
weekpointerStaleness r wp =
    case r of
        ScheduleRoute _ (Just wq) ->
            if wq == wp.curr then
                Nothing

            else
                Just wq

        _ ->
            Nothing


refreshStaleWeekpointer : Model -> Cmd Msg
refreshStaleWeekpointer m =
    case weekpointerStaleness m.route m.focus of
        Just w ->
            getWeekpointer <| encodeWeek w

        _ ->
            Cmd.none


wptrlater500ms : Debounce.Config Msg
wptrlater500ms =
    { strategy = Debounce.later 1000
    , transform = WeekpointerDebounced
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString url)
                        ]
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = toRoute url }
            , Cmd.batch
                [ refreshStaleWeekpointer model
                , case toRoute url of
                    BookingsRoute ->
                        callBookings
                    HostsRoute filter ->
                        if Hosts.isEmpty model.hostsdata
                        then callHosts model.apiBaseUrl filter Nothing
                        else Cmd.none

                    _ ->
                        Cmd.none
                ]
            )

        GotWeekpointer w ->
            case w of
                Err e ->
                    ( { model
                        | timesCall = Error (Decode.errorToString e)
                      }
                    , Cmd.none
                    )

                Ok wp ->
                    let
                        ( d, c ) =
                            Debounce.push wptrlater500ms wp.window model.lastwptr
                    in
                    ( { model | focus = wp, lastwptr = d }
                    , c
                    )

        WeekpointerDebounced w ->
            let
                getTimes x =
                    Maybe.map (callTimes model.apiBaseUrl x) (routeHostId model.route)
                        |> Maybe.withDefault Cmd.none

                ( d, c ) =
                    Debounce.update
                        wptrlater500ms
                        (Debounce.takeLast getTimes)
                        w
                        model.lastwptr
            in
            ( { model | lastwptr = d }, c )
        
        HostsFilterInput f ->
            ( { model | hostsdata = Hosts.recordHostFilterInput model.hostsdata f }
            , Cmd.none
            )

        HostsFilterQuery ->
            ( { model | hostsdata = Hosts.recordFilterQuery model.hostsdata }
            , Route.setFilterParameter model.key model.route model.hostsdata.filter
            )

        FetchHosts filter page ->
            ( { model | hostsdata = Hosts.requestData model.hostsdata }
            , callHosts model.apiBaseUrl filter page )


        HostsFetched (Err (Http.BadStatus 401)) ->
            ( { model | sessionState = Session.recordStaleness }
            , Cmd.none
            )

        HostsFetched (Err e) ->
            ( { model 
                | hostsdata = receiveData (Error (toMsg e)) model.hostsdata 
                }
            , Cmd.none
            )

        HostsFetched (Ok hs) ->
            ( { model
                | hostsdata = receiveData (Response hs) model.hostsdata
              }
            , Cmd.none
            )

        AppointmentsFetched (Err (Http.BadStatus 401)) ->
            ( { model
                | sessionState = Session.recordStaleness
              }
            , Cmd.none
            )

        AppointmentsFetched (Err e) ->
            ( { model | timesCall = Error <| toMsg e }
            , Cmd.none
            )

        AppointmentsFetched (Ok res) ->
            ( { model
                | timesCall = Response res
                , bookCallHistory = Dict.empty
              }
            , getTimesClock (Encode.list encodeAppointment res)
            )

        GotTimesClock (Ok res) ->
            ( { model | timesCall = Response res }
            , Cmd.none
            )

        GotTimesClock (Err e) ->
            ( model, Cmd.none )

        GotBookingsClock (Ok res) ->
            ( { model | bookingsCall = Response res }, Cmd.none )

        GotBookingsClock (Err e) ->
            ( { model | bookingsCall = Error "" }, Cmd.none )

        NeedsCreds ->
            ( model, Nav.load <| UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (routeToUrl model.route) ] )

        Book b ->
            ( { model | bookCallHistory = Dict.insert b.start Pending model.bookCallHistory }, callBook b )

        MeetBooked ( ts, Ok b ) ->
            ( { model | bookCallHistory = Dict.insert b.start (Response b) model.bookCallHistory }
            , Cmd.batch [ callBookings ]
            )

        MeetBooked ( ts, Err (Http.BadStatus 401) ) ->
            ( { model
                | sessionState = Session.recordStaleness
                , bookCallHistory = Dict.insert ts (Error "Need to re-login.") model.bookCallHistory
              }
            , Cmd.none
            )

        MeetBooked ( ts, Err e ) ->
            ( { model
                | bookCallHistory = Dict.insert ts (Error (toMsg e)) model.bookCallHistory
              }
            , Cmd.none
            )

        Unbook r ->
            ( { model
                | unbookCallHistory = Dict.insert r Pending model.unbookCallHistory
              }
            , callUnbook r
            )

        Unbooked ( ts, Err (Http.BadStatus 401) ) ->
            ( { model
                | sessionState = Session.recordStaleness
                , unbookCallHistory = Dict.insert ts (Error "Need to login") model.unbookCallHistory
              }
            , Cmd.none
            )

        Unbooked ( ts, Err e ) ->
            ( { model
                | unbookCallHistory = Dict.insert ts (Error (toMsg e)) model.unbookCallHistory
              }
            , Cmd.none
            )

        Unbooked ( ts, Ok _ ) ->
            ( { model
                | unbookCallHistory = Dict.insert ts (Response ()) model.unbookCallHistory
              }
            , Cmd.none
            )

        GotBookings (Ok bs) ->
            ( { model
                | bookingsCall = Response bs
                , unbookCallHistory = Dict.empty
              }
            , getBookingsClock (Encode.list encodeAppointment bs)
            )

        GotBookings (Err (Http.BadStatus 401)) ->
            ( { model | sessionState = Session.recordStaleness }
            , Cmd.none
            )

        GotBookings (Err e) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ gotWeekpointer (GotWeekpointer << Decode.decodeValue decodeWeekpointer)
        , gotTimesClock (GotTimesClock << Decode.decodeValue decodeAppointments)
        , gotBookingsClock (GotBookingsClock << Decode.decodeValue decodeAppointments)
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "meets - public client"
    , body =
        [ div [ class "root-view" ] (routeToView model)
        ]
    }


routeToView : Model -> List (Html Msg)
routeToView m =
    case m.route of
        NotFound ->
            [ div [ class "component" ] [ p [] [ text "not found" ] ] ]

        HomeRoute p ->
            [ div homeLinkStyle homelink
            , div sessionStateStyle (sessionstateView m.route m.csrf m.sessionState)
            , if Session.isSignedIn m.sessionState then
                div routeLinkStyle (bookingsLink m)

              else
                text ""
            , div routeLinkStyle hostsLink
            ]

        HostsRoute f ->
            div homeLinkStyle homelink
                :: div sessionStateStyle (sessionstateView m.route m.csrf m.sessionState)
                :: div routeLinkStyle hostsAnchor
                :: div Hosts.hostsFilterStyle Hosts.hostsFilter
                :: Hosts.hostsDataView m.route m.hostsdata

        ScheduleRoute h p ->
            []

        BookingsRoute ->
            [ div homeLinkStyle homelink
            , div sessionStateStyle (sessionstateView m.route m.csrf m.sessionState)
            , div routeLinkStyle bookingsAnchor
            ]


homeLinkStyle : List (Attribute Msg)
homeLinkStyle =
    [ class "pt-1em"
    , class "heavy-bkg"
    ]


routeLinkStyle : List (Attribute Msg)
routeLinkStyle =
    [ class "pt-b-2em"
    , class "light-bkg"
    , class "light-sep"
    ]


homelink : List (Html Msg)
homelink =
    [ a
        [ href "/"
        , class "alt-txt-col"
        , class "large-text"
        ]
        [ text "meets" ]
    ]



hostsLink : List (Html Msg)
hostsLink =
    [ a
        [ href "hosts"
        , class "main-txt-col"
        , class "large-text"
        , class "routeLink"
        ]
        [ text "hosts" ]
    ]


hostsAnchor : List (Html Msg)
hostsAnchor =
    [ p
        [ class "main-txt-col"
        , class "large-text"
        ]
        [ text "hosts" ]
    ]


bookingsLink : Model -> List (Html Msg)
bookingsLink m =
    [ a
        [ href "/bookings"
        , class "main-txt-col"
        , class "large-text"
        , class "routeLink"
        ]
        [ text "bookings" ]
    ]


bookingsAnchor : List (Html Msg)
bookingsAnchor =
    [ p
        [ class "main-txt-col"
        , class "large-text"
        ]
        [ text "bookings" ]
    ]


sessionStateStyle : List (Attribute Msg)
sessionStateStyle =
    [ class "pt-b-05em"
    , class "heavy-bkg"
    ]









-- hosts












-- Header





-- sessionControl : Model -> Html Msg
-- sessionControl m =
--     case m.sessionState of
--         Fresh n ->
--             Html.nav [ class "sessionControl" ]
--                 [ text (String.concat [ "Signed in: ", n, " ▼" ])
--                 , ul []
--                     [ li [] [ logoutTrigger m ] ]
--                 ]

--         _ ->
--             text ""



-- Side
-- myBookings


-- myBookings : Model -> Html Msg
-- myBookings m =
--     if isSignedIn m.sessionState then
--         bookingsCount m

--     else
--         a
--             [ onClick <| NeedsCreds
--             , class "login-to-book"
--             ]
--             [ text "Sign in" ]


mybookingsUrl : String
mybookingsUrl =
    UrlB.absolute [ "bookings" ] []


bookingsCount : Model -> Html Msg
bookingsCount m =
    a
        [ class "myBookingsLink"
        , href mybookingsUrl
        ]
        [ dl
            [ class "myBookings" ]
            [ dt [] [ text "booked" ]
            , dd [] [ bookigCountView m ]
            ]
        ]


bookigCountView : Model -> Html Msg
bookigCountView m =
    case m.bookingsCall of
        Uncalled ->
            text "-"

        Pending ->
            text "."

        Response n ->
            text <| String.fromInt <| List.length n

        Error _ ->
            text "!"



-- content : Model -> List (Html Msg)
-- content m =
--     case m.route of
--         HomeRoute _ ->
--             []

--         HostsRoute _ ->
--             []

--         ScheduleRoute _ _ ->
--             weekPointerView m.route m.focus :: times m

--         BookingsRoute ->
--             myBookingsListing m

--         NotFound ->
--             []



-- myBookings


-- myBookingsListing : Model -> List (Html Msg)
-- myBookingsListing m =
--     [ h2 [] [ text "My bookings" ]
--     , bookingsView m
--     ]


-- bookingsView : Model -> Html Msg
-- bookingsView m =
--     case ( isSignedIn m.sessionState, m.bookingsCall ) of
--         ( True, Response ts ) ->
--             ul [] <|
--                 List.map
--                     (bookingView m.sessionState m.unbookCallHistory)
--                     ts

--         ( False, _ ) ->
--             p [] [ text "Your session has become stale. You need to sign in again." ]

--         _ ->
--             text ""


unBookBtn : Int -> Html Msg
unBookBtn start =
    button [ onClick (Unbook start) ] [ text "UnBook" ]


-- bookingView : SessionState -> UnbookCommandHistory -> Appointment -> Html Msg
-- bookingView s history appt =
--     li [ class "meetitem" ]
--         [ dl [ class "meetdata" ]
--             [ dt [] [ text "name" ]
--             , dd [] [ text appt.hostRef ]
--             , dt [] [ text "start" ]
--             , dd [] [ text (Maybe.withDefault "" appt.oclock) ]
--             , dt [] [ text "duration" ]
--             , dd [] [ text (duration appt.dur) ]
--             ]
--         , case ( isSignedIn s, Dict.get appt.start history ) of
--             ( False, _ ) ->
--                 text ""

--             ( _, Just Pending ) ->
--                 button [ class "ofinterest" ] [ text "" ]

--             ( _, Just (Response _) ) ->
--                 button [ class "ofinterest" ] [ text "Unbooked." ]

--             ( _, Just (Error msg) ) ->
--                 button [ class "ofinterest" ] [ text msg ]

--             ( _, Nothing ) ->
--                 button [ onClick (Unbook appt.start) ] [ text "Unbook" ]

--             _ ->
--                 text ""
--         ]



--week=2019-45


addWeekFocusQuery : Route -> Week -> String
addWeekFocusQuery r w =
    let
        query =
            UrlB.string "week" w
    in
    case r of
        ScheduleRoute s _ ->
            UrlB.absolute [ "hosts", s ] [ query ]

        _ ->
            UrlB.absolute [] [ query ]


weekPointerView : Route -> WeekPointer -> Html Msg
weekPointerView r { prev, curr, next, window } =
    let
        ( y, w ) =
            case String.split "-" curr of
                [ year, week ] ->
                    ( year, week )

                _ ->
                    ( "", "" )
    in
    nav [ class "weekpointer" ]
        [ a [ href <| addWeekFocusQuery r prev, id "weekpointer-left" ] [ text "◀" ]
        , p [ id "weekpointer-year" ] [ text y ]
        , p [ id "weekpointer-week" ] [ text w ]
        , a [ href <| addWeekFocusQuery r next, id "weekpointer-right" ] [ text "▶" ]
        ]



-- Times


-- times : Model -> List (Html Msg)
-- times m =
--     case m.timesCall of
--         Uncalled ->
--             [ h2 [] [ text "no host selected" ] ]

--         Pending ->
--             [ h2 [] [ text "..." ] ]

--         Response s ->
--             appointmentListView s m

--         Error e ->
--             [ h2 [] [ text e ] ]


-- appointmentListView : List Appointment -> Model -> List (Html Msg)
-- appointmentListView ts m =
--     case m.route of
--         ScheduleRoute _ _ ->
--             [ ul [] (List.map (appointmentView m.sessionState m.bookCallHistory) ts)
--             ]

--         _ ->
--             [ p [] [ text "No timezone." ] ]


-- appointmentView : SessionState -> BookCommandHistory -> Appointment -> Html Msg
-- appointmentView s history appt =
--     li [ class "meetitem" ]
--         [ dl [ class "meetdata" ]
--             [ dt [] [ text "name" ]
--             , dd [] [ text appt.hostRef ]
--             , dt [] [ text "start" ]
--             , dd [] [ text (Maybe.withDefault "" appt.oclock) ]
--             , dt [] [ text "duration" ]
--             , dd [] [ text (duration appt.dur) ]
--             ]
--         , case ( isSignedIn s, Dict.get appt.start history ) of
--             ( False, _ ) ->
--                 button [ onClick NeedsCreds ] [ text "Sign in to book." ]

--             ( _, Just Pending ) ->
--                 button [ class "ofinterest" ] [ text "" ]

--             ( _, Just (Response _) ) ->
--                 button [ class "ofinterest" ] [ text "Booked." ]

--             ( _, Just (Error msg) ) ->
--                 button [ class "ofinterest" ] [ text msg ]

--             ( _, Nothing ) ->
--                 button [ onClick (Book appt) ] [ text "Book" ]

--             _ ->
--                 text ""
--         ]


duration : Int -> String
duration d =
    String.concat [ String.fromInt d, " ", "min" ]
