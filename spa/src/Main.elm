port module Main exposing (ApiBaseUrl, ApiCall(..), Appointment, End, Flags, Host, HostId, HostRef, Minutes, Model, Msg(..), Oclock, Route(..), SessionState(..), Start, UnixTs, Week, WeekPointer, Window, addWeekFocusQuery, bookigCountView, bookingsCount, bookingsView, callBook, callBookings, callTimes, callUnbook, content, decodeAppointment, decodeAppointments, decodeHost, decodeHosts, decodeTimesWindow, decodeWeekpointer, duration, encodeAppointment, encodeWeek, getBookingsClock, getTimesClock, getWeekpointer, gotBookingsClock, gotTimesClock, gotWeekpointer, homelink, host, hostSwitch, hosts, init, isSignedIn, loadHosts, logoutTrigger, logoutUrl, main, myBookings, myBookingsListing, mybookingsUrl, refreshStaleWeekpointer, route, routeHostId, routeToUrl, sessionControl, subscriptions, times, toMsg, toRoute, unBookBtn, update, view, weekPointerView, weekpointerStaleness)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Debounce as Debounce exposing (Debounce)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Encode as Encode exposing (Value)
import Url
import Url.Builder as UrlB
import Url.Parser as UrlP exposing ((</>), (<?>))
import Url.Parser.Query as Query
import Dict as Dict exposing (Dict)



-- Times
-- Error


toMsg : Http.Error -> String
toMsg err =
    case err of
        Http.BadUrl s ->
            "Invalid url: " ++ s

        Http.Timeout ->
            "Request timed out."

        Http.NetworkError ->
            "Network error."

        Http.BadStatus s ->
            "Bad status: " ++ String.fromInt s

        Http.BadBody s ->
            "Bad body: " ++ s



-- Hosts


type alias Host =
    { id : String
    , name : String
    , timezone : String
    }


decodeHost : Decoder Host
decodeHost =
    Decode.map3 Host
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "tz" Decode.string)


decodeHosts : Decoder (List Host)
decodeHosts =
    Decode.list decodeHost



-- Appointment


type alias HostId =
    String


type alias HostRef =
    String


type alias UnixTs =
    Int


type alias Oclock =
    String


type alias Minutes =
    Int


type alias Appointment =
    { hostId : HostId
    , hostRef : HostRef
    , start : UnixTs
    , oclock : Maybe Oclock
    , dur : Minutes
    }


port gotTimesClock : (Value -> msg) -> Sub msg


port gotBookingsClock : (Value -> msg) -> Sub msg


decodeAppointment : Decoder Appointment
decodeAppointment =
    Decode.map5 Appointment
        (Decode.field "hostId" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "start" Decode.int)
        (Decode.maybe (Decode.field "oclock" Decode.string))
        (Decode.field "dur" Decode.int)


decodeAppointments : Decoder (List Appointment)
decodeAppointments =
    Decode.list decodeAppointment


port getTimesClock : Value -> Cmd a


port getBookingsClock : Value -> Cmd a


encodeAppointment : Appointment -> Value
encodeAppointment m =
    Encode.object
        [ ( "hostId", Encode.string m.hostId )
        , ( "name", Encode.string m.hostRef )
        , ( "start", Encode.int m.start )
        , ( "oclock"
          , case m.oclock of
                Just c ->
                    Encode.string c

                _ ->
                    Encode.null
          )
        , ( "dur", Encode.int m.dur )
        ]


type SessionState
    = Fresh String
    | Stale
    | None


isSignedIn : SessionState -> Bool
isSignedIn s =
    case s of
        Fresh _ ->
            True

        _ ->
            False

-- Route


type Route
    = NotFound
    | HomeRoute (Maybe String)
    | ScheduleRoute String (Maybe String)
    | BookingsRoute


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound <| UrlP.parse route url


route =
    UrlP.oneOf
        [ UrlP.map HomeRoute (UrlP.top <?> Query.string "week")
        , UrlP.map ScheduleRoute (UrlP.s "hosts" </> UrlP.string <?> Query.string "week")
        , UrlP.map BookingsRoute (UrlP.s "bookings")
        ]


logoutUrl : Model -> String
logoutUrl m =
    UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (routeToUrl m.route) ]


routeToUrl : Route -> String
routeToUrl r =
    case r of
        ScheduleRoute s w ->
            UrlB.absolute [ "hosts", s ] (Maybe.map (\x -> [ UrlB.string "week" x ]) w |> Maybe.withDefault [])

        _ ->
            UrlB.absolute [] []


routeHostId : Route -> Maybe HostId
routeHostId r =
    case r of
        ScheduleRoute id _ ->
            Just id

        _ ->
            Nothing



-- week-pointer


type alias Week =
    String


type alias Start =
    Int


type alias End =
    Int


type alias Window =
    ( Start, End )


type alias WeekPointer =
    { prev : Week
    , curr : Week
    , next : Week
    , window : Window
    }


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



-- Api


type ApiCall a
    = Uncalled
    | Pending
    | Response a
    | Error String


type alias ApiBaseUrl =
    String



-- Host (Cross origin - considered public)
-- /hosts


loadHosts : Route -> ApiBaseUrl -> Cmd Msg
loadHosts r m =
    case r of
        NotFound ->
            Cmd.none

        _ ->
            Http.get
                { url = UrlB.crossOrigin m [ "hosts" ] []
                , expect = Http.expectJson HostsFetched decodeHosts
                }



-- host/{hostId}/times?from={from}&to={to}


callTimes : ApiBaseUrl -> Window -> HostId -> Cmd Msg
callTimes base ( from, to ) hostId =
    Http.get
        { url =
            UrlB.crossOrigin base
                [ "hosts", hostId, "times" ]
                [ UrlB.int "from" from
                , UrlB.int "to" to
                ]
        , expect = Http.expectJson AppointmentsFetched decodeAppointments
        }



-- POST /bookings


callBook : Appointment -> Cmd Msg
callBook m =
  let
      ctor = MeetBooked << Tuple.pair m.start
  in
    Http.post
        { url = UrlB.absolute [ "api", "bookings" ] []
        , body = Http.jsonBody <| encodeAppointment m
        , expect = Http.expectJson ctor decodeAppointment
        }



-- DELETE /bookings


callUnbook : Int -> Cmd Msg
callUnbook r =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = UrlB.absolute [ "api", "bookings", String.fromInt r ] []
        , body = Http.emptyBody
        , expect = Http.expectWhatever (Unbooked << Tuple.pair r)
        , timeout = Nothing
        , tracker = Nothing
        }



-- /bookings


callBookings : Cmd Msg
callBookings =
    Http.get
        { url = UrlB.absolute [ "api", "bookings" ] []
        , expect = Http.expectJson GotBookings decodeAppointments
        }

type alias BookCommandRecord = ( UnixTs, ApiCall Appointment ) 
type alias UnbookCommandRecord = ( UnixTs, ApiCall () )
type alias BookCommandHistory = Dict UnixTs (ApiCall Appointment) 
type alias UnbookCommandHistory = Dict UnixTs (ApiCall ())

-- MODEL


type alias Model =
    { key : Nav.Key
    , csrf : String
    , route : Route
    , sessionState : SessionState
    , focus : WeekPointer
    , lastwptr : Debounce Window
    , apiBaseUrl : ApiBaseUrl
    , hostsCall : ApiCall (List Host)
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
      , sessionState =
            Maybe.map Fresh flags.name
                |> Maybe.withDefault None
      , focus = flags.focus
      , lastwptr = Debounce.init
      , apiBaseUrl = flags.apiBaseUrl
      , hostsCall = Pending
      , timesCall =
            Maybe.map (always Pending) hostId
                |> Maybe.withDefault Uncalled
      , bookingsCall = Uncalled
      , bookCallHistory = Dict.empty
      , unbookCallHistory = Dict.empty
      }
    , Cmd.batch
        [ loadHosts (toRoute url) flags.apiBaseUrl
        , Maybe.map (always callBookings) flags.name
            |> Maybe.withDefault Cmd.none
        , Maybe.map (callTimes flags.apiBaseUrl flags.focus.window) hostId
            |> Maybe.withDefault Cmd.none
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | WeekpointerDebounced Debounce.Msg
    | HostsFetched (Result Http.Error (List Host))
    | GotWeekpointer (Result Decode.Error WeekPointer)
    | AppointmentsFetched (Result Http.Error (List Appointment))
    | GotTimesClock (Result Decode.Error (List Appointment))
    | GotBookingsClock (Result Decode.Error (List Appointment))
    | NeedsCreds
    | Book Appointment
    | MeetBooked (UnixTs, Result Http.Error Appointment)
    | Unbook Int
    | Unbooked (UnixTs, (Result Http.Error ()))
    | GotBookings (Result Http.Error (List Appointment))
    | NoOp


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
                    ( { model
                        | route = toRoute url
                      }
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString url)
                        , Maybe.map (callTimes model.apiBaseUrl model.focus.window) (hostSwitch url model)
                            |> Maybe.withDefault Cmd.none
                        ]
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( model
            , Cmd.batch
                [ refreshStaleWeekpointer model
                , case model.route of
                    BookingsRoute -> callBookings
                    _             -> Cmd.none
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

        HostsFetched (Err (Http.BadStatus 401)) ->
            ( { model | sessionState = Stale }
            , Cmd.none
            )

        HostsFetched (Err e) ->
            ( { model | timesCall = Error <| toMsg e }
            , Cmd.none
            )

        HostsFetched (Ok ss) ->
            ( { model | hostsCall = Response ss }
            , Cmd.none
            )

        AppointmentsFetched (Err (Http.BadStatus 401)) ->
            ( { model 
              | sessionState = Stale }
            , Cmd.none
            )

        AppointmentsFetched (Err e) ->
            ( { model | timesCall = Error <| toMsg e }
            , Cmd.none
            )

        AppointmentsFetched (Ok res) ->
            ( { model 
            | timesCall = Response res
            , bookCallHistory = Dict.empty }
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

        MeetBooked (ts, Ok b) ->
            ( { model | bookCallHistory = Dict.insert b.start (Response b) model.bookCallHistory }
            , Cmd.batch [ callBookings ]
            )

        MeetBooked (ts, Err (Http.BadStatus 401)) ->
            ( { model 
              | sessionState = Stale
              , bookCallHistory = Dict.insert ts (Error "Need to re-login.") model.bookCallHistory}
            , Cmd.none
            )

        MeetBooked (ts, Err e) ->
            ( { model 
              | bookCallHistory = Dict.insert ts (Error (toMsg e)) model.bookCallHistory }
            , Cmd.none
            )

        Unbook r ->
            ( { model 
            | unbookCallHistory = Dict.insert r Pending model.unbookCallHistory }
            , callUnbook r )

        Unbooked (ts, (Err (Http.BadStatus 401))) ->
            ( { model 
              | sessionState = Stale
              , unbookCallHistory = Dict.insert ts (Error "Need to login") model.unbookCallHistory }
            , Cmd.none
            )

        Unbooked (ts, Err e) ->
            ( { model 
              | unbookCallHistory = Dict.insert ts (Error (toMsg e)) model.unbookCallHistory 
              }
              , Cmd.none )

        Unbooked (ts, Ok _) ->
            ( { model 
              | unbookCallHistory = Dict.insert ts (Response ()) model.unbookCallHistory 
              }
              , Cmd.none )

        GotBookings (Ok bs) ->
            ( { model
                | bookingsCall = Response bs
                , unbookCallHistory = Dict.empty
              }
            , getBookingsClock (Encode.list encodeAppointment bs)
            )

        GotBookings (Err (Http.BadStatus 401)) ->
            ( { model | sessionState = Stale }
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
        [ div [ class "layout" ]
            [ div [ class "homelink" ] [ homelink ]
            , div [ class "header" ] [ sessionControl model ]
            , div [ class "side" ]
                [ myBookings model
                , h1 [] [ text "Hosts" ]
                , hosts model
                ]
            , div [ class "content" ] (content model)
            ]
        ]
    }



-- Homelink


homelink : Html Msg
homelink =
    a
        [ href "/"
        ]
        [ text "meets" ]



-- Header


logoutTrigger : Model -> Html Msg
logoutTrigger m =
    Html.form
        [ action (logoutUrl m), method "post" ]
        [ input
            [ type_ "submit"
            , value "Logout"
            , class "logoutTrigger"
            ]
            []
        , input
            [ type_ "hidden"
            , name "__RequestVerificationToken"
            , value m.csrf
            ]
            []
        ]


sessionControl : Model -> Html Msg
sessionControl m =
    case m.sessionState of
        Fresh n ->
            Html.nav [ class "sessionControl" ]
                [ text (String.concat [ "Signed in: ", n, " ▼" ])
                , ul []
                    [ li [] [ logoutTrigger m ] ]
                ]

        _ ->
            text ""



-- Side
-- myBookings


myBookings : Model -> Html Msg
myBookings m =
    if isSignedIn m.sessionState then
        bookingsCount m

    else
        a
            [ onClick <| NeedsCreds
            , class "login-to-book"
            ]
            [ text "Sign in" ]


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



-- host-list


hosts : Model -> Html Msg
hosts m =
    case m.hostsCall of
        Uncalled ->
            text ""

        Pending ->
            div [ class "pending" ] [ text "" ]

        Response hs ->
            ul [ class "schedules-list" ] (List.map (host m) hs)

        Error e ->
            i [] [ text e ]


host : Model -> Host -> Html Msg
host m h =
    let
        rs r =
            case r of
                ScheduleRoute s _ ->
                    Just s

                _ ->
                    Nothing

        selected =
            Maybe.map ((==) h.id) (rs m.route)
                |> Maybe.withDefault False
    in
    li [ classList [ ( "selected", selected ) ] ]
        [ a [ href (routeToUrl <| ScheduleRoute h.id Nothing) ] [ text h.name ]
        , p [] [ text h.timezone ]
        ]



-- Content view


content : Model -> List (Html Msg)
content m =
    case m.route of
        HomeRoute _ ->
            []

        ScheduleRoute _ _ ->
            weekPointerView m.route m.focus :: times m

        BookingsRoute ->
            myBookingsListing m

        NotFound ->
            []



-- myBookings


myBookingsListing : Model -> List (Html Msg)
myBookingsListing m =
    [ h2 [] [ text "My bookings" ]
    , bookingsView m
    ]


bookingsView : Model -> Html Msg
bookingsView m =
    case ( isSignedIn m.sessionState, m.bookingsCall ) of
        ( True, Response ts ) ->
            ul [] <|
                List.map
                    (bookingView m.sessionState m.unbookCallHistory)
                    ts

        ( False, _ ) ->
            p [] [ text "Your session has become stale. You need to sign in again." ]

        _ ->
            text ""


unBookBtn : Int -> Html Msg
unBookBtn start =
    button [ onClick (Unbook start) ] [ text "UnBook" ]

bookingView : SessionState -> UnbookCommandHistory -> Appointment -> Html Msg
bookingView s history appt =
    li [ class "meetitem" ]
        [ dl [ class "meetdata" ]
            [ dt [] [ text "name" ]
            , dd [] [ text appt.hostRef ]
            , dt [] [ text "start" ]
            , dd [] [ text (Maybe.withDefault "" appt.oclock) ]
            , dt [] [ text "duration" ]
            , dd [] [ text (duration appt.dur) ]
            ]
        , case (isSignedIn s, Dict.get appt.start history) of
            (False, _) -> text ""
            (_, Just Pending) -> button [ class "ofinterest"] [ text "" ]
            (_, Just (Response _)) -> button [ class "ofinterest" ] [ text "Unbooked." ]
            (_, Just (Error msg)) -> button [ class "ofinterest" ] [ text msg ]
            (_, Nothing) -> button [ onClick (Unbook appt.start) ] [ text "Unbook" ]
            _ -> text ""
        ]



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


times : Model -> List (Html Msg)
times m =
    case m.timesCall of
        Uncalled ->
            [ h2 [] [ text "no host selected" ] ]

        Pending ->
            [ h2 [] [ text "..." ] ]

        Response s ->
            appointmentListView s m

        Error e ->
            [ h2 [] [ text e ] ]


appointmentListView : List Appointment -> Model -> List (Html Msg)
appointmentListView ts m =
    case m.route of
        ScheduleRoute _ _ ->
            [ ul [] (List.map ( appointmentView m.sessionState m.bookCallHistory ) ts)
            ]

        _ ->
            [ p [] [ text "No timezone." ] ]


appointmentView : SessionState -> BookCommandHistory -> Appointment -> Html Msg
appointmentView s history appt =
    li [ class "meetitem" ]
        [ dl [ class "meetdata" ]
            [ dt [] [ text "name" ]
            , dd [] [ text appt.hostRef ]
            , dt [] [ text "start" ]
            , dd [] [ text (Maybe.withDefault "" appt.oclock) ]
            , dt [] [ text "duration" ]
            , dd [] [ text (duration appt.dur) ]
            ]
        , case (isSignedIn s, Dict.get appt.start history) of
            (False, _) -> button [ onClick NeedsCreds ] [ text "Sign in to book." ]
            (_, Just Pending) -> button [ class "ofinterest"] [ text "" ]
            (_, Just (Response _)) -> button [ class "ofinterest" ] [ text "Booked." ]
            (_, Just (Error msg)) -> button [ class "ofinterest" ] [ text msg ]
            (_, Nothing) -> button [ onClick (Book appt) ] [ text "Book" ]
            _ -> text ""
        ]


duration : Int -> String
duration d =
    String.concat [ String.fromInt d, " ", "min" ]
