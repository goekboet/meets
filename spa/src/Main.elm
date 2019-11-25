port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Browser.Dom as Dom
import Html exposing (..)
import Html.Events exposing (onClick, onFocus)
import Html.Attributes exposing (..)
import Url
import Url.Builder as UrlB
import Url.Parser as UrlP exposing ((</>), (<?>))
import Url.Parser.Query as Query
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Encode as Encode exposing (Value)
import Task exposing ( perform )
import Debug

-- Times


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

-- Error

toMsg : Http.Error -> String
toMsg err =
  case err of
    Http.BadUrl s -> "Invalid url: " ++ s
    Http.Timeout  -> "Request timed out."
    Http.NetworkError -> "Network error."
    Http.BadStatus s -> "Bad status: " ++ String.fromInt s
    Http.BadBody s -> "Bad body: " ++ s

-- Hosts

type alias Host =
  { id : String
  , name : String
  , timezone : String
  }

decodeHost : Decoder Host
decodeHost = Decode.map3 Host
  (Decode.field "id" Decode.string)
  (Decode.field "name" Decode.string)
  (Decode.field "tz" Decode.string)

decodeHosts : Decoder (List Host)
decodeHosts =  Decode.list decodeHost

-- Appointment

type alias HostId = String
type alias HostRef = String
type alias UnixTs = Int
type alias Oclock = String
type alias Minutes = Int

type alias Appointment =
  { hostId : HostId
  , hostRef : HostRef
  , start : UnixTs
  , oclock: (Maybe Oclock)
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
      [ ( "hostId", Encode.string m.hostId)
      , ( "name", Encode.string m.hostRef )
      , ( "start", Encode.int m.start )
      , ( "oclock", 
          (case m.oclock of 
            Just c -> Encode.string c
            _ -> Encode.null))
      , ( "dur", Encode.int m.dur )
      ]

type SessionState
  = Fresh String
  | Stale
  | None

isSignedIn : SessionState -> Bool
isSignedIn s = case s of
  Fresh _ -> True
  _     -> False

-- MODEL

type alias Model =
  { key : Nav.Key
  , csrf: String
  , route : Route
  , sessionState: SessionState
  , focus: WeekPointer
  , apiBaseUrl : ApiBaseUrl
  , hostsResult: HostResult
  , timesResult: TimesResult
  , myBookingsResult: BookingsRsult 
  , notifications: List Notification
  }

type HostResult =
  Fetching
  | Fetched (List Host)
  | Errored String

type TimesResult =
  UnFetched
  | FetchingTimes
  | FetchedTimes (List Appointment)
  | ErroredTimes String

type BookedResult =
  NotBooked
  | BookedSuccess UnixTs
  | BookedError String

type BookingsRsult =
  NotAvailiable
  | BookingSuccess (List Appointment)
  | BookingError 

-- Init

type alias ApiBaseUrl = String

type alias Flags =
  { name : Maybe String
  , csrf: String
  , focus : WeekPointer
  , apiBaseUrl : ApiBaseUrl
  }

isNull : Maybe a -> Bool
isNull x = case x of  
  Just _ -> True
  _ -> False 


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { key = key
    , csrf = flags.csrf
    , route = toRoute url
    , sessionState = Maybe.map Fresh flags.name
      |> Maybe.withDefault None
    , focus = flags.focus
    , apiBaseUrl = flags.apiBaseUrl
    , hostsResult = Fetching
    , timesResult = UnFetched 
    , notifications = []
    , myBookingsResult = NotAvailiable
    }
  , Cmd.batch 
    [ loadHosts (toRoute url) (flags.apiBaseUrl)
    , loadBookings (isNull flags.name)
    , callTimes (flags.apiBaseUrl) (flags.focus.window) (toRoute url) ]
  )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | HostsFetched (Result Http.Error (List Host))
  | GotWeekpointer (Result Decode.Error WeekPointer)
  | TimesFetched (Result Http.Error (List Appointment))
  | GotTimesClock (Result Decode.Error (List Appointment))
  | GotBookingsClock (Result Decode.Error (List Appointment))
  | NeedsCreds 
  | Book Appointment
  | MeetBooked (Result Http.Error Appointment)
  | UnBook Int
  | TimeUnbooked (Result Http.Error ())
  | MeetConfirmed Notification
  | DiscardConfirmation NotificationIndex
  | GotBookings (Result Http.Error (List Appointment))
  | NoOp



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( { model 
            | route = toRoute url }
          , Cmd.batch 
            [ Nav.pushUrl model.key (Url.toString url)
            , if hostSwitch url model 
              then callTimes model.apiBaseUrl model.focus.window (toRoute url) 
              else Cmd.none
            ]
          )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( model
      , Cmd.batch
        [ refreshStaleWeekpointer model
        ]
      )

    GotWeekpointer w ->
      case w of
        Err e -> 
          (
            { model 
            | timesResult = ErroredTimes (Decode.errorToString e)
            }
          , Cmd.none
          )
        Ok wp -> 
          ( { model | focus = wp }
          , callTimes model.apiBaseUrl wp.window model.route
          )
    HostsFetched (Err (Http.BadStatus 401) ) ->
      ( { model | sessionState = Stale }
      , Cmd.none
      )

    HostsFetched (Err e) ->
      ( { model | timesResult = ErroredTimes <| toMsg e}
      , Cmd.none
      )

    HostsFetched (Ok ss) ->
      ( receiveSchedules model ss
      , Cmd.none
      )

    TimesFetched (Err (Http.BadStatus 401) )  ->
      ( { model | sessionState = Stale }
      , Cmd.none
      )

    TimesFetched (Err e) ->
      ( { model | timesResult = ErroredTimes <| toMsg e }
      , Cmd.none )
    

    TimesFetched (Ok res) ->
      ( receiveSchedule model res
      , getTimesClock (Encode.list encodeAppointment res) 
      )

    GotTimesClock (Ok res) ->
      ( receiveSchedule model res, Cmd.none )

    GotTimesClock (Err e) ->
      (model, Cmd.none)

    GotBookingsClock (Ok res) ->
      ( { model | myBookingsResult = BookingSuccess res }, Cmd.none )

    GotBookingsClock (Err e) ->
      ( { model | myBookingsResult = BookingError }, Cmd.none)
    
    NeedsCreds ->
      ( model, Nav.load <| UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (routeToUrl model.route) ] )

    Book b ->
      ( model, callBook b )
    
    MeetBooked (Ok b) ->
      ( model 
      , Cmd.batch [ callBookings ] 
      ) 

    MeetBooked (Err (Http.BadStatus 401) )  ->
      ( { model | sessionState = Stale }
      , Cmd.none
      ) 

    MeetBooked (Err e) ->
      ( addNotification (toMsg e) model, Cmd.none ) 

    UnBook r ->
      (model, callUnbook r )

    TimeUnbooked (Err (Http.BadStatus 401) )  ->
      ( { model | sessionState = Stale }
      , Cmd.none
      )

    TimeUnbooked (Err e) ->
      (addNotification (toMsg e) model, Cmd.none)

    TimeUnbooked (Ok _) ->
      (model, callBookings )

    MeetConfirmed c ->
      ( { model 
        | notifications = c :: model.notifications 
        }
      , Cmd.none )        
    
    DiscardConfirmation t ->
      ( discardNotification t model, Cmd.none )

    GotBookings (Ok bs) ->
      ( { model 
        | myBookingsResult = BookingSuccess bs
        }
      , getBookingsClock (Encode.list encodeAppointment bs) 
      )

    GotBookings (Err (Http.BadStatus 401) )  ->
      ( { model | sessionState = Stale }
      , Cmd.none
      )

    GotBookings (Err e) ->
      ( model, Cmd.none )
    
    NoOp ->
      ( model, Cmd.none )

-- Host

hostUrl : ApiBaseUrl -> String
hostUrl base = UrlB.crossOrigin base ["hosts"] []

receiveSchedule : Model -> (List Appointment) -> Model
receiveSchedule m s =
  { m | timesResult = FetchedTimes s }

loadHosts : Route -> ApiBaseUrl -> Cmd Msg
loadHosts r m =
  case r of
    NotFound -> Cmd.none
    _        -> Http.get
      { url = hostUrl m
      , expect = Http.expectJson HostsFetched decodeHosts }

receiveSchedules : Model -> (List Host) -> Model
receiveSchedules m ss =
  { m | hostsResult = Fetched ss }

-- Bookings

callBook : Appointment -> Cmd Msg
callBook m =
  Http.post
    { url = UrlB.absolute [ "api", "bookings" ] []
    , body = Http.jsonBody <| encodeAppointment m
    , expect = Http.expectJson MeetBooked decodeAppointment
    }

callUnbook : Int -> Cmd Msg
callUnbook r =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = UrlB.absolute [ "api", "bookings", String.fromInt r ] []
    , body = Http.emptyBody
    , expect = Http.expectWhatever TimeUnbooked 
    , timeout = Nothing
    , tracker = Nothing
    }

loadBookings : Bool -> Cmd Msg
loadBookings b =
  if b
    then
      callBookings
    else
      Cmd.none

callBookings : Cmd Msg
callBookings = Http.get
  { url = UrlB.absolute [ "api", "bookings" ] []
  , expect = Http.expectJson GotBookings decodeAppointments}

-- Notification

type alias Notification = String

type alias NotificationIndex = Int

addNotification : Notification -> Model -> Model
addNotification n m = { m | notifications = n :: m.notifications }

discardNotification : NotificationIndex -> Model -> Model
discardNotification t m =
  let
    before = List.take t m.notifications
    after = List.drop 1 before    
  in
    { m | notifications = List.concat [ before, after ] }


-- Times

hostSwitch : Url.Url -> Model -> Bool
hostSwitch url model =
  case (toRoute url, model.route) of
      (ScheduleRoute nH _, ScheduleRoute oH _) -> nH /= oH
      (ScheduleRoute _  _, _                 ) -> True
      (_                 , ScheduleRoute _  _) -> False
      _                                        -> False

callTimes : ApiBaseUrl -> Window -> Route -> Cmd Msg
callTimes base (from, to) r =
  case r of
    ScheduleRoute s _ -> Http.get
      { url = UrlB.crossOrigin base 
        [ "hosts", s, "times" ] 
        [ UrlB.int "from" from
        , UrlB.int "to" to
        ]
      , expect = Http.expectJson TimesFetched decodeAppointments
      }
    _ -> Cmd.none

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch
  [ gotWeekpointer (GotWeekpointer << Decode.decodeValue decodeWeekpointer)
  , gotTimesClock (GotTimesClock << Decode.decodeValue decodeAppointments)
  , gotBookingsClock (GotBookingsClock << Decode.decodeValue decodeAppointments)
  ]
  

-- Route

type Route 
  = NotFound
  | HomeRoute (Maybe String)
  | ScheduleRoute String (Maybe String)
  | BookingsRoute

toRoute : Url.Url -> Route
toRoute url = Maybe.withDefault NotFound <| UrlP.parse route url

route =
  UrlP.oneOf 
    [ UrlP.map HomeRoute (UrlP.top <?> Query.string "week")
    , UrlP.map ScheduleRoute (UrlP.s "hosts" </> UrlP.string <?> Query.string "week")
    , UrlP.map BookingsRoute (UrlP.s "bookings")]

logoutUrl : Model -> String
logoutUrl m =
  UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (routeToUrl m.route) ]

routeToUrl : Route -> String
routeToUrl r = case r of
  ScheduleRoute s w -> UrlB.absolute [ "hosts", s ] (Maybe.map (\x -> [UrlB.string "week" x]) w |> Maybe.withDefault [])
  _                 -> UrlB.absolute [] []

routeHostId : Route -> Maybe HostId
routeHostId r = case r of
  ScheduleRoute id _ -> Just id
  _                  -> Nothing

view : Model -> Browser.Document Msg
view model =
  { title = "meets - public client"
  , body = [ div [ class "layout" ] 
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
homelink = a [  href "/" 
             ]
             [ text "meets" ]

-- Header

logoutTrigger : Model -> Html Msg
logoutTrigger m = Html.form 
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
      Html.nav [ class "sessionControl"] 
        [ text (String.concat [ "Signed in as: ", n, " ▼"])
        , ul []
          [ li [] [ logoutTrigger m ] ]
        ]
    _ -> text ""

-- Side

-- myBookings

myBookings : Model -> Html Msg
myBookings m = if isSignedIn m.sessionState
        then bookingsCount m 
        else a [ onClick <| NeedsCreds 
               , class "login-to-book"] [ text "Sign in" ] 

mybookingsUrl : String
mybookingsUrl = UrlB.absolute [ "bookings" ] []

bookingsCount : Model -> Html Msg
bookingsCount m = 
  a [ class "myBookingsLink" 
    , href mybookingsUrl
    ]
    [ dl 
      [ class "myBookings" ] 
      [ dt [] [ text "booked" ]
      , dd [] [ bookigCountView m.myBookingsResult ]
      ]
    ] 

bookigCountView : BookingsRsult -> Html Msg
bookigCountView r =
  case r of
    NotAvailiable -> text "-" 
    BookingSuccess n -> text <| String.fromInt <| List.length n 
    BookingError -> text "!" 

-- host-list

hosts : Model -> Html Msg
hosts m =
  case m.hostsResult of
    Fetching   -> text ""
    Fetched ss -> ul [ class "schedules-list" ] (List.map (host m) ss) 
    Errored e  -> i [] [text e] 
          
host : Model -> Host -> Html Msg
host m h =
  let
      rs r = case r of
        ScheduleRoute s _ -> Just s
        _                 -> Nothing

      selected = Maybe.map ((==) h.id) (rs m.route)
        |> Maybe.withDefault False
  in
    li [ classList [("selected", selected)] ] 
       [ a [ href (routeToUrl <| ScheduleRoute h.id Nothing)] [ text h.name ] 
       , p [] [ text h.timezone]
       ]
   
-- Content view 

content : Model -> List (Html Msg)
content m = case m.route of
  HomeRoute _ -> []
  ScheduleRoute _ _ -> weekPointerView m.route m.focus :: times m
  BookingsRoute -> myBookingsListing m
  NotFound -> []

-- myBookings
myBookingsListing : Model -> List (Html Msg)
myBookingsListing m =
  [ h2 [] [ text "My bookings"]
  , bookingsView m
  ]

bookingsView : Model -> Html Msg
bookingsView m =
  case (isSignedIn m.sessionState, m.myBookingsResult) of
    (True, BookingSuccess ts) ->
      ul [] (List.map
        (\b -> 
          li [ class "meetitem" ]
            [ dl [ class "meetdata"] 
              [ dt [] [ text "name" ]
              , dd [] [ text b.hostRef ]
              , dt [] [ text "start"]
              , dd [] [ text (Maybe.withDefault "" b.oclock) ]
              , dt [] [ text "duration"]
              , dd [] [ text (String.fromInt b.dur) ]
              ]
            , unBookBtn b.start
            ]
        ) ts)
    (False, _) -> p [] [ text "Your session has become stale. You need to sign in again."] 
    _ -> text ""
unBookBtn : Int -> Html Msg
unBookBtn start =
  button [ onClick (UnBook start) ] [ text "UnBook" ]

-- week-pointer
type alias Week = String
type alias Start = Int
type alias End = Int
type alias Window = (Start, End)
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

refreshStaleWeekpointer : Model -> Cmd Msg
refreshStaleWeekpointer m =
  case weekpointerStaleness m.route m.focus of
    Just w -> getWeekpointer <| encodeWeek w
    _ -> Cmd.none

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

--week=2019-45
addWeekFocusQuery : Route -> Week -> String
addWeekFocusQuery r w =
  let
    query = UrlB.string "week" w 
  in
    case r of
      ScheduleRoute s _ -> UrlB.absolute [ "hosts", s ] [ query ]
      _                 -> UrlB.absolute [] [ query ]

weekpointerStaleness : Route -> WeekPointer -> (Maybe Week)
weekpointerStaleness r wp =
  case r of
    ScheduleRoute _ (Just wq) -> if wq == wp.curr then Nothing else (Just wq)
    _ -> Nothing

zeroPadw2 : Int -> String
zeroPadw2 n = 
  let
      w =  String.append "0" (String.fromInt n)
  in
    String.right 2 w
  

weekPointerView : Route -> WeekPointer -> Html Msg
weekPointerView r { prev, curr, next, window } =
  let
      (y, w) = case String.split "-" curr of
        [ year, week ] -> (year, week)
        _ -> ("", "")
      
  in
    nav [ class "weekpointer" ] 
      [ a [ href  <| addWeekFocusQuery r prev, id "weekpointer-left" ] [ text "◀" ] 
      , p [ id "weekpointer-year" ] [ text y ]
      , p [ id "weekpointer-week" ] [ text w ]
      , a [ href <| addWeekFocusQuery r next, id "weekpointer-right" ] [ text "▶" ]
      ]
  
-- Times

times : Model -> List (Html Msg)
times m =
  case m.timesResult of
    UnFetched -> [ h2 [] [ text "no host selected"] ]
    FetchingTimes -> [ h2 [] [ text "..."] ]
    FetchedTimes s -> timesView s m
    ErroredTimes e -> [ h2 [] [ text e ] ]

timesView : (List Appointment) -> Model -> List (Html Msg) 
timesView ts m =
  case m.route of
    (ScheduleRoute _ _) -> 
      [ ul [] (List.map (timeView <| isSignedIn m.sessionState) ts) 
      ]
    _                                 -> [ p [] [ text "No timezone." ] ]

timeView : Bool -> Appointment -> Html Msg
timeView creds appt = 
  li [ class "meetitem" ]
    [ dl [ class "meetdata"] 
      [ dt [] [ text "name" ]
      , dd [] [ text appt.hostRef ]
      , dt [] [ text "start"]
      , dd [] [ text (Maybe.withDefault "" appt.oclock) ]
      , dt [] [ text "duration"]
      , dd [] [ text (duration appt.dur) ]
      ]
    , bookView creds appt
    ]

duration : Int -> String
duration d =
  String.concat [ String.fromInt d, " ", "min" ]

bookView : Bool -> Appointment -> Html Msg
bookView hasCreds m =
  let
    focus =  classList [ ]   
  in
    if hasCreds
      then button [ focus, onClick (Book m) ] [ text "Book" ] 
      else button [ focus, onClick NeedsCreds ] [ text "Sign in to book." ]
