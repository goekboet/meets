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
import Time as T exposing (Posix)
import Task exposing ( perform )
import Debug
--import Week as W exposing (WeekName)
-- Ports


  




-- Times

timeStamp : T.Zone -> Posix -> String
timeStamp z t = 
  String.join " "
    [ String.join "-" 
      [ String.fromInt <| T.toYear z t 
      , monthNames <| T.toMonth z t
      , String.padLeft 2 '0' <| String.fromInt <| T.toDay z t
      ]
    , String.join ":" 
      [ String.padLeft 2 '0' <| String.fromInt <| T.toHour z t
      , String.padLeft 2 '0' <| String.fromInt <| T.toMinute z t
      ]
    ]

monthNames : T.Month -> String 
monthNames m =
  case m of
    T.Jan -> "01"
    T.Feb -> "02"
    T.Mar -> "03"
    T.Apr -> "04"
    T.May -> "05"
    T.Jun -> "06"
    T.Jul -> "07"
    T.Aug -> "08"
    T.Sep -> "09"
    T.Oct -> "10"
    T.Nov -> "11"
    T.Dec -> "12"

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
  (Decode.field "hostId" Decode.string)
  (Decode.field "hostName" Decode.string)
  (Decode.field "timeZone" Decode.string)

decodeHosts : Decoder (List Host)
decodeHosts =  Decode.list decodeHost

-- Meets

type alias Time =
  { hostId : String
  , meetName : String
  , start: Int
  , dur: Int
  }

decodeTime : Decoder Time
decodeTime = Decode.map4 Time
    (Decode.field "hostId" Decode.string)
    (Decode.field "meetName" Decode.string)
    (Decode.field "start" Decode.int)
    (Decode.field "dur" Decode.int)

decodeTimes : Decoder (List Time)
decodeTimes = 
  Decode.list decodeTime

encodeTime : Time -> Value
encodeTime m = Encode.object 
  [ ( "hostId", Encode.string m.hostId)
  , ( "meetName", Encode.string m.meetName )
  , ( "start", Encode.int m.start )
  , ( "dur", Encode.int m.dur )
  ]

-- Book
type alias BookingRequest = 
  { hostId : String
  , start : Int
  , timeId : String
  }

encodeBookingRequest : BookingRequest -> Value
encodeBookingRequest r = Encode.object
  [ ( "hostId", Encode.string r.hostId )
  , ( "start", Encode.int r.start )
  , ( "timeId", Encode.string r.timeId)
  ]

decodeBookingRequest : Decoder BookingRequest
decodeBookingRequest = Decode.map3 BookingRequest
  (Decode.field "hostId" Decode.string)
  (Decode.field "start" Decode.int)
  (Decode.field "timeId" Decode.string)

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
  , initTime : Posix
  , focus: WeekPointer
  , hostsResult: HostResult
  , timesResult: TimesResult
  , myBookingsResult: BookingsRsult 
  , notifications: List BookingConfirm
  }

type HostResult =
  Fetching
  | Fetched (List Host)
  | Errored String

type TimesResult =
  UnFetched
  | FetchingTimes
  | FetchedTimes (List Time)
  | ErroredTimes String

type alias Reason = (String, Posix)
type alias ConfirmedMeet = (String, Posix)

type BookingConfirm 
  = Affirmative ConfirmedMeet
  | Decline Reason 

type BookedResult =
  NotBooked
  | BookedSuccess Time
  | BookedError String

type BookingsRsult =
  NotAvailiable
  | BookingSuccess (List Time)
  | BookingError 

-- Init

type alias Flags =
  { name : Maybe String
  , csrf: String
  , now : Int
  , focus : WeekPointer
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
    , hostsResult = Fetching
    , timesResult = UnFetched 
    , notifications = []
    , myBookingsResult = NotAvailiable
    , initTime = T.millisToPosix flags.now
    }
  , Cmd.batch 
    [ loadHosts (toRoute url)
    , loadBookings (isNull flags.name)
    , callTimes (flags.focus.window) (toRoute url) ]
  )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | HostsFetched (Result Http.Error (List Host))
  | GotTimesWindow (Result Decode.Error WeekPointer)
  | TimesFetched (Result Http.Error (List Time))
  | NeedsCreds 
  | Book BookingRequest
  | MeetBooked (Result Http.Error BookingRequest)
  | UnBook Int
  | TimeUnbooked (Result Http.Error ())
  | MeetConfirmed BookingConfirm
  | DiscardConfirmation Posix
  | GotBookings (Result Http.Error (List Time))
  | NoOp



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( { model 
            | route = toRoute url }
          , Nav.pushUrl model.key (Url.toString url) 
          )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( model 
      , Cmd.batch
        [ refreshStaleWeekpointer model
        ]
      )

    GotTimesWindow w ->
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
          , callTimes wp.window model.route
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
      ( receiveSchedule model res, Cmd.none )
    
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
      ( model, addDecline (toMsg e) ) 

    UnBook r ->
      (model, callUnbook r )

    TimeUnbooked (Err (Http.BadStatus 401) )  ->
      ( { model | sessionState = Stale }
      , Cmd.none
      )

    TimeUnbooked (Err e) ->
      (model, addDecline (toMsg e) )

    TimeUnbooked (Ok _) ->
      (model, callBookings )

    MeetConfirmed c ->
      ( { model 
        | notifications = c :: model.notifications 
        }
      , Cmd.none )        
    
    DiscardConfirmation t ->
      ( discardConfirmation t model, Cmd.none )

    GotBookings (Ok bs) ->
      ( { model 
        | myBookingsResult = BookingSuccess bs
        }
      , Cmd.none 
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

hostUrl : String
hostUrl = UrlB.crossOrigin "https://broker.ego" ["hosts"] []

receiveSchedule : Model -> (List Time) -> Model
receiveSchedule m s =
  { m | timesResult = FetchedTimes s }

loadHosts : Route -> Cmd Msg
loadHosts  r =
  case r of
    NotFound -> Cmd.none
    _        -> Http.get
      { url = hostUrl
      , expect = Http.expectJson HostsFetched decodeHosts }

receiveSchedules : Model -> (List Host) -> Model
receiveSchedules m ss =
  { m | hostsResult = Fetched ss }

-- Bookings

callBook : BookingRequest -> Cmd Msg
callBook m =
  Http.post
    { url = UrlB.absolute [ "api", "bookings" ] []
    , body = Http.jsonBody <| encodeBookingRequest m
    , expect = Http.expectJson MeetBooked decodeBookingRequest
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
  , expect = Http.expectJson GotBookings decodeTimes}

-- Bookingconfirmation

-- addConfirmation : Model -> BookingRequest -> Cmd Msg 
-- addConfirmation m meet =
--   case m.timezone of
--     Just tz -> 
--       let
--         timestamp = Tuple.pair 
--       in
--         Task.perform (MeetConfirmed << Affirmative << timestamp (bookingConfirmation tz meet)) T.now
--     _ -> Cmd.none

addDecline : String -> Cmd Msg
addDecline r =
  let
      timestamp = Tuple.pair
  in
    Task.perform (MeetConfirmed << Decline << timestamp r) T.now

bookingConfirmation : T.Zone -> BookingRequest -> String
bookingConfirmation tz m =
  let
      start = timeStamp tz (T.millisToPosix m.start) 
  in
      String.join " " [ m.timeId, "at", start ]

discardConfirmation : Posix -> Model -> Model
discardConfirmation t m =
  let
      match c = case c of
            Affirmative (_, s) -> t == s
            Decline (_, s) -> t == s
  in
   { m | notifications = List.filter (not << match) m.notifications }


-- Meets

callTimes : Window -> Route -> Cmd Msg
callTimes (from, to) r =
  case r of
    ScheduleRoute s _ -> Http.get
      { url = UrlB.crossOrigin "https://broker.ego" 
        [ "hosts", s, "times" ] 
        [ UrlB.int "from" from
        , UrlB.int "to" to
        ]
      , expect = Http.expectJson TimesFetched decodeTimes
      }
    _ -> Cmd.none

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  gotWeekpointer (GotTimesWindow << Decode.decodeValue decodeWeekpointer)

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
  ScheduleRoute s _ -> UrlB.absolute [ "hosts", s ] []
  _                 -> UrlB.absolute [] []




addMeetFragment : String -> String
addMeetFragment m =
  (UrlB.custom UrlB.Relative [] [] (Just m))

-- getWeekName : Route -> Maybe (Int, Int)
-- getWeekName r = case r of
--   ScheduleRoute _ (Just w) -> W.s2WeekOrder w
--   HomeRoute (Just w)       -> W.s2WeekOrder w
--   _                        -> Nothing
-- View


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
   
-- message-record

messageRecord : List BookingConfirm -> T.Zone -> Html Msg
messageRecord bs tz = 
  if List.isEmpty bs 
    then text "" 
    else ul [ class "messagerecord" ] (List.map (li [] << confirmView tz) bs)

confirmView : T.Zone -> BookingConfirm -> List (Html Msg)
confirmView z c =
  case c of
    Affirmative (m, t) -> 
      [ dl [] 
        [ dt [] [ text (timeStamp z t) ]
        , dd [] [ text m ]
        ]
      , button [ onClick (DiscardConfirmation t) ] [] 
      ]
    Decline (r, t) ->
      [ dl [] 
        [ dt [] [ text (timeStamp z t) ]
        , dd [] [ text r ]
        ]
      , button [ onClick (DiscardConfirmation t) ] [] 
      ]

-- content

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
              , dd [] [ text b.meetName ]
              , dt [] [ text "start"]
              , dd [] [ text (String.fromInt b.start) ]
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
-- type alias Year = String
-- type alias WeekNumber = String
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

timesView : (List Time) -> Model -> List (Html Msg) 
timesView ts m =
  case m.route of
    (ScheduleRoute _ _) -> 
      [ ul [] (List.map (timeView <| isSignedIn m.sessionState) ts) 
      ]
    _                                 -> [ p [] [ text "No timezone." ] ]

timeView : Bool -> Time -> Html Msg
timeView creds m = 
  li [ class "meetitem" ]
    [ dl [ class "meetdata"] 
      [ dt [] [ text "name" ]
      , dd [] [ text m.meetName ]
      , dt [] [ text "start"]
      , dd [] [ text (String.fromInt m.start) ]
      , dt [] [ text "duration"]
      , dd [] [ text (duration m.dur) ]
      ]
    , bookView creds (BookingRequest m.hostId m.start m.meetName)
    ]


duration : Int -> String
duration d =
  String.concat [ String.fromInt d, " ", "min" ]

bookView : Bool -> BookingRequest -> Html Msg
bookView hasCreds m =
  let
    focus =  classList [ ]   
  in
    if hasCreds
      then button [ focus, onClick (Book m) ] [ text "Book" ] 
      else button [ focus, onClick NeedsCreds ] [ text "Sign in to book." ]
