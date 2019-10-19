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
import Week as W exposing (WeekName)

-- Ports

port getTimesWindow : Value -> Cmd a

encodeTimesWindowInput : Maybe (String) -> Value
encodeTimesWindowInput s =
  let
      wo = Maybe.andThen W.s2WeekOrder s
  in
    case wo of
      Just (y, w) -> Encode.list Encode.int [y, w]
      Nothing -> Encode.null
  

port gotTimesWindow : (Value -> msg) -> Sub msg

decodeTimesWindow : Decoder (Int, Int)
decodeTimesWindow =
  Decode.map2 Tuple.pair (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)

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
  }

decodeHost : Decoder Host
decodeHost = Decode.map2 Host
  (Decode.field "hostId" Decode.string)
  (Decode.field "hostName" Decode.string)

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
  = Fresh
  | Stale
  | None

isSignedIn : SessionState -> Bool
isSignedIn s = case s of
  Fresh -> True
  _     -> False

-- MODEL

type alias Model =
  { key : Nav.Key
  , route : Route
  , sessionState: SessionState
  , initTime : Posix
  , timezone: Maybe T.Zone
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
  { hasCreds : Bool
  , now : Int
  }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { key = key
    , route = toRoute url
    , sessionState = if flags.hasCreds then Fresh else None
    , timezone = Nothing
    , hostsResult = Fetching
    , timesResult = UnFetched 
    , notifications = []
    , myBookingsResult = NotAvailiable
    , initTime = T.millisToPosix flags.now
    }
  , Cmd.batch 
    [ loadHosts (toRoute url)
    , getTimesWindowFromRoute (toRoute url)
    , perform AgentZone T.here
    , loadBookings flags.hasCreds
    ]
  )

getTimesWindowFromRoute : Route -> Cmd a
getTimesWindowFromRoute r =
  case r of
    ScheduleRoute _ wo -> getTimesWindow <| encodeTimesWindowInput wo
    _                  -> Cmd.none

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | AgentZone T.Zone
  | HostsFetched (Result Http.Error (List Host))
  | GotTimesWindow (Result Decode.Error (Int, Int))
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
        [ getTimesWindowFromRoute model.route
        ]
      )

    AgentZone z ->
      ( { model | timezone = (Just z) }
      , Cmd.none
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
        Ok wo -> 
          ( model
          , callTimes wo model.route
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
      , Cmd.batch [ callBookings, getTimesWindowFromRoute model.route ] 
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
      , getTimesWindowFromRoute model.route)        
    
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

addConfirmation : Model -> BookingRequest -> Cmd Msg 
addConfirmation m meet =
  case m.timezone of
    Just tz -> 
      let
        timestamp = Tuple.pair 
      in
        Task.perform (MeetConfirmed << Affirmative << timestamp (bookingConfirmation tz meet)) T.now
    _ -> Cmd.none

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

callTimes : (Int, Int) -> Route -> Cmd Msg
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
  gotTimesWindow (GotTimesWindow << Decode.decodeValue decodeTimesWindow)

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

getWeekName : Route -> Maybe (Int, Int)
getWeekName r = case r of
  ScheduleRoute _ (Just w) -> W.s2WeekOrder w
  HomeRoute (Just w)       -> W.s2WeekOrder w
  _                        -> Nothing
-- View


view : Model -> Browser.Document Msg
view model =
  { title = "meets - public client"
  , body = [ div [ class "layout" ] 
             [ div [ class "homelink" ] [ homelink ]
             , div [ class "header" ] [ logoutTrigger model ]
             , div [ class "side" ] 
               [ myBookings model
               , h1 [] [ text "Hosts" ]
               , hosts model
               , if List.isEmpty model.notifications then text "" else h1 [] [ text "Confirmed" ] 
               , Maybe.map (messageRecord model.notifications) model.timezone
                   |> Maybe.withDefault (text "")
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
logoutTrigger m =
  if isSignedIn m.sessionState
  then Html.form [ action (logoutUrl m), method "post" ] [ input [ type_ "submit", value "Logout" ] []]
  else text ""

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

nextYear : (Int, Int) -> String
nextYear (y, w) =
  String.concat
      [ String.padLeft 4 '0' (String.fromInt (y + 1))
      , "-"
      , String.padLeft 2 '0' (String.fromInt w)
      ]
  
lastYear : (Int, Int) -> String
lastYear (y, w) =
  String.concat
      [ String.padLeft 4 '0' (String.fromInt (y - 1))
      , "-"
      , String.padLeft 2 '0' (String.fromInt w)
      ]

nextWeek : (Int, Int) -> String
nextWeek c =
  let
      (y, w) = W.nextWeek c 
  in
    String.concat
      [ String.padLeft 4 '0' (String.fromInt y)
      , "-"
      , String.padLeft 2 '0' (String.fromInt w)
      ]

lastWeek : (Int, Int) -> String
lastWeek c =
  let
      (y, w) = W.lastWeek c
  in
    String.concat
      [ String.padLeft 4 '0' (String.fromInt y)
      , "-"
      , String.padLeft 2 '0' (String.fromInt w)
      ]

stepWeekPointer : Route -> String -> String
stepWeekPointer r w = case r of
  ScheduleRoute s _ -> UrlB.absolute [ "hosts", s ] [ UrlB.string "week" w ]
  HomeRoute _       -> UrlB.absolute [] [ UrlB.string "week" w ]
  _                 -> UrlB.absolute [] []



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
       [ a [ href (routeToUrl <| ScheduleRoute h.id Nothing)] [ text h.name ] ]
   
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
  ScheduleRoute _ _ -> weekPointer m :: times m
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
  case (m.timezone, isSignedIn m.sessionState, m.myBookingsResult) of
    (Just tz, True, BookingSuccess ts) ->
      ul [] (List.map
        (\b -> 
          li [ class "meetitem" ]
            [ dl [ class "meetdata"] 
              [ dt [] [ text "name" ]
              , dd [] [ text b.meetName ]
              , dt [] [ text "start"]
              , dd [] [ text (startTime tz b.start) ]
              , dt [] [ text "duration"]
              , dd [] [ text (String.fromInt b.dur) ]
              ]
            , unBookBtn b.start
            ]
        ) ts)
    (_, False, _) -> p [] [ text "Your session has become stale. You need to sign in again."] 
    _ -> text ""

unBookBtn : Int -> Html Msg
unBookBtn start =
  button [ onClick (UnBook start) ] [ text "UnBook" ]

-- week-pointer
weekPointer : Model -> Html Msg
weekPointer m =
  case m.timezone of
      Nothing -> text ""
      Just tz ->
        let
            (y, w) = Maybe.withDefault (W.posix2Week tz m.initTime)
                      <| getWeekName m.route
            
            fmt n = String.padLeft n '0' << String.fromInt
        in
          div [ class "weekpointer" ] 
              [ a [ class "back", href (stepWeekPointer m.route (lastYear (y, w))) ] [ text "◀" ]
              , i [ class "label" ] [ text (fmt 4 y) ]
              , a [ class "fwd", href (stepWeekPointer m.route (nextYear (y, w))) ] [ text "▶"]
              , a [ class "back", href (stepWeekPointer m.route (lastWeek (y, w))) ] [ text "◀"]
              , i [ class "label"] [ text (fmt 2 w) ]
              , a [ class "fwd", href (stepWeekPointer m.route (nextWeek (y, w))) ] [ text "▶"]
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
  case (m.timezone, m.route) of
    (Just tz, ScheduleRoute _ _) -> 
      [ ul [] (List.map (timeView tz <| isSignedIn m.sessionState) ts) 
      ]
    _                                 -> [ p [] [ text "No timezone." ] ]

timeView : T.Zone -> Bool -> Time -> Html Msg
timeView z creds m = 
  li [ class "meetitem" ]
    [ dl [ class "meetdata"] 
      [ dt [] [ text "name" ]
      , dd [] [ text m.meetName ]
      , dt [] [ text "start"]
      , dd [] [ text (startTime z m.start) ]
      , dt [] [ text "duration"]
      , dd [] [ text (duration m.dur) ]
      ]
    , bookView creds (BookingRequest m.hostId m.start m.meetName)
    ]

startTime : T.Zone -> Int -> String
startTime z t =
  let
      p = T.millisToPosix t
      h = String.padLeft 2 '0' << String.fromInt << T.toHour z
      m = String.padLeft 2 '0' << String.fromInt << T.toMinute z 
  in
    String.concat 
      [ h p
      , ":"
      , m p
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
