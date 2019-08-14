module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Browser.Dom as Dom
import Html exposing (..)
import Html.Events exposing (onClick, onFocus)
import Html.Attributes exposing (..)
import Url
import Url.Builder as UrlB
import Url.Parser as UrlP exposing ((</>))
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Encode as Encode exposing (Value)
import Time as T exposing (Posix)
import Task exposing ( perform )
import Debug

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

-- Schedules

type alias Schedule =
  { host : String
  , meets : List Meet
  }

decodeSchedule : Decoder Schedule
decodeSchedule =
  Decode.map2 Schedule 
    (Decode.field "host" Decode.string)
    (Decode.field "meets" decodeMeets)

decodeSchedules : Decoder (List String)
decodeSchedules =  Decode.list (Decode.field "name" Decode.string)

-- Meets

type alias Meet =
  { schedule : String
  , name : String
  , time: Int
  , dur: Int
  }

decodeMeet : Decoder Meet
decodeMeet = Decode.map4 Meet
    (Decode.field "schedule" Decode.string)
    (Decode.field "name" Decode.string)
    (Decode.field "time" Decode.int)
    (Decode.field "duration" Decode.int)

decodeMeets : Decoder (List Meet)
decodeMeets = 
  Decode.list decodeMeet

encodeMeet : Meet -> Value
encodeMeet m = Encode.object 
  [ ( "schedule", Encode.string m.schedule)
  , ( "name", Encode.string m.name )
  , ( "time", Encode.int m.time )
  , ( "duration", Encode.int m.dur )
  ]

-- MODEL

type alias Model =
  { key : Nav.Key
  , route : Route
  , hasCreds: Bool
  , timezone: Maybe T.Zone
  , schedules: Schedules
  , schedule: ScheduleResult
  , confirmedBookings: List BookingConfirm
  , bookingCount: BookingCountResult 
  }

type Schedules =
  Fetching
  | Fetched (List String)
  | Errored String

type ScheduleResult =
  NoSchedule
  | FetchingSchedule
  | FetchedSchedule Schedule
  | ErroredSchedule String

type alias Reason = (String, Posix)
type alias ConfirmedMeet = (String, Posix)

type BookingConfirm 
  = Affirmative ConfirmedMeet
  | Decline Reason 

type BookedResult =
  NotBooked
  | BookedSuccess Meet
  | BookedError String

type BookingCountResult =
  NotAvailiable
  | BookingCountSuccess Int
  | BookingCountError 

-- Init

type alias Flags =
  { hasCreds : Bool
  }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { key = key
    , route = toRoute url
    , hasCreds = flags.hasCreds
    , timezone = Nothing
    , schedules = Fetching
    , schedule = NoSchedule 
    , confirmedBookings = []
    , bookingCount = NotAvailiable
    }
  , Cmd.batch 
    [ loadSchedules (toRoute url)
    , loadMeets (toRoute url)
    , perform AgentZone T.here
    , loadBookings flags.hasCreds
    ]
  )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | AgentZone T.Zone
  | FetchedSchedules (Result Http.Error (List String))
  | FetchedMeets (Result Http.Error Schedule)
  | NeedsCreds 
  | ScrollAttempted (Result Dom.Error ())
  | Book Meet
  | MeetBooked (Result Http.Error Meet)
  | MeetConfirmed BookingConfirm
  | DiscardConfirmation Posix
  | GotBookings (Result Http.Error (List Meet))
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
        [ callMeets model.route
        ]
      )

    AgentZone z ->
      ( { model | timezone = (Just z) }
      , Cmd.none
      )

    FetchedSchedules (Err e) ->
      ( { model | schedule = ErroredSchedule <| toMsg e}
      , Cmd.none
      )

    FetchedSchedules (Ok ss) ->
      ( receiveSchedules model ss
      , Cmd.none
      )

    FetchedMeets (Err e) ->
      ( { model | schedule = ErroredSchedule <| toMsg e }, Cmd.none )

    FetchedMeets (Ok res) ->
      ( receiveSchedule model res, scrollToMeet model.route )
    
    NeedsCreds ->
      ( model, Nav.load <| UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (routeToUrl model.route) ] )

    ScrollAttempted r -> ( model, Cmd.none )

    Book b ->
      ( model, callBook b )
    
    MeetBooked (Ok b) ->
      ( model 
      , Cmd.batch 
        [ addConfirmation model b
        , callBookings] 
      ) 

    MeetBooked (Err e) ->
      ( model, addDecline (toMsg e) ) 

    MeetConfirmed c ->
      ( { model 
        | confirmedBookings = c :: model.confirmedBookings 
        }
      , loadMeets model.route)        
    
    DiscardConfirmation t ->
      ( discardConfirmation t model, Cmd.none )

    GotBookings (Ok bs) ->
      ( { model 
        | bookingCount = BookingCountSuccess <| List.length bs
        }
      , Cmd.none 
      )

    GotBookings (Err e) ->
      ( model, Cmd.none )
    
    NoOp ->
      ( model, Cmd.none )

-- Host

receiveSchedule : Model -> Schedule -> Model
receiveSchedule m s =
  { m | schedule = FetchedSchedule s }

loadSchedules : Route -> Cmd Msg
loadSchedules  r =
  case r of
    NotFound -> Cmd.none
    _        -> Http.get
      { url = UrlB.absolute [ "api", "schedules" ] []
      , expect = Http.expectJson FetchedSchedules decodeSchedules }

receiveSchedules : Model -> (List String) -> Model
receiveSchedules m ss =
  { m | schedules = Fetched ss }

-- Bookings

callBook : Meet -> Cmd Msg
callBook m =
  Http.post
    { url = UrlB.absolute [ "api", "bookings" ] []
    , body = Http.jsonBody <| encodeMeet m
    , expect = Http.expectJson MeetBooked decodeMeet
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
  , expect = Http.expectJson GotBookings decodeMeets}

-- Bookingconfirmation

addConfirmation : Model -> Meet -> Cmd Msg 
addConfirmation m meet =
  case m.timezone of
    Just tz -> 
      let
        timestamp = Tuple.pair 
      in
        Task.perform (MeetConfirmed << Affirmative << timestamp (meetConfirmation tz meet)) T.now
    _ -> Cmd.none

addDecline : String -> Cmd Msg
addDecline r =
  let
      timestamp = Tuple.pair
  in
    Task.perform (MeetConfirmed << Decline << timestamp r) T.now

meetConfirmation : T.Zone -> Meet -> String
meetConfirmation tz m =
  let
      start = timeStamp tz (T.millisToPosix m.time) 
  in
      String.join " " [ m.name, "at", start ]

discardConfirmation : Posix -> Model -> Model
discardConfirmation t m =
  let
      match c = case c of
            Affirmative (_, s) -> t == s
            Decline (_, s) -> t == s
  in
   { m | confirmedBookings = List.filter (not << match) m.confirmedBookings }


-- Meets

callMeets : Route -> Cmd Msg
callMeets r =
  case r of
    ScheduleRoute s Nothing -> Http.get
      { url = UrlB.absolute [ "api", "schedules", s ] []
      , expect = Http.expectJson FetchedMeets decodeSchedule
      }
    _ -> Cmd.none

scrollToMeet : Route -> Cmd Msg
scrollToMeet r =
  case r of
    ScheduleRoute _ (Just fgmt) -> 
      Task.attempt ScrollAttempted
      (Dom.getElement fgmt
        |> Task.andThen (\info -> Dom.setViewport 0 info.element.y))
    _                           -> Cmd.none

loadMeets : Route -> Cmd Msg
loadMeets r =
  case r of
    ScheduleRoute s _ -> Http.get
      { url = UrlB.absolute [ "api", "schedules", s ] []
      , expect = Http.expectJson FetchedMeets decodeSchedule
      }
    _ -> Cmd.none

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- Route

type Route 
  = NotFound
  | HomeRoute
  | ScheduleRoute String (Maybe String)

toRoute : Url.Url -> Route
toRoute url = Maybe.withDefault NotFound <| UrlP.parse route url

route =
  UrlP.oneOf 
    [ UrlP.map HomeRoute UrlP.top
    , UrlP.map ScheduleRoute (UrlP.string </> UrlP.fragment identity)
    ]

logoutUrl : Model -> String
logoutUrl m =
  UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (routeToUrl m.route) ]

routeToUrl : Route -> String
routeToUrl r = case r of
  ScheduleRoute s Nothing  -> UrlB.absolute [ s ] []
  ScheduleRoute s (Just m) -> UrlB.custom UrlB.Absolute [ s ] [] (Just m)
  _                               -> UrlB.absolute [] []

addMeetFragment : String -> String
addMeetFragment m =
  (UrlB.custom UrlB.Relative [] [] (Just m))


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
               , schedules model
               , if List.isEmpty model.confirmedBookings then text "" else h1 [] [ text "Confirmed" ] 
               , Maybe.map (messageRecord model.confirmedBookings) model.timezone
                   |> Maybe.withDefault (text "")
               ]
             , div [ class "content" ] (meets model)
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
  if m.hasCreds
  then Html.form [ action (logoutUrl m), method "post" ] [ input [ type_ "submit", value "Logout" ] []]
  else text ""

-- Side

-- myBookings

myBookings : Model -> Html Msg
myBookings m = if m.hasCreds
        then bookingsCount m 
        else a [ onClick <| NeedsCreds 
               , class "login-to-book"] [ text "Let me book for you" ] 

bookingsCount : Model -> Html Msg
bookingsCount m = dl [ class "myBookings" ] 
           [ dt [] [ text "booked" ]
           , dd [] [ bookigCountView m.bookingCount ]
           ] 

bookigCountView : BookingCountResult -> Html Msg
bookigCountView r =
  case r of
    NotAvailiable -> text "-" 
    BookingCountSuccess n -> text <| String.fromInt n 
    BookingCountError -> text "!" 

-- host-list

schedules : Model -> Html Msg
schedules m =
  case m.schedules of
    Fetching   -> text ""
    Fetched ss -> ul [ class "schedules-list" ] (List.map (schedule m) ss) 
    Errored e  -> i [] [text e] 
          
schedule : Model -> String -> Html Msg
schedule m n =
  let
      rs r = case r of
        ScheduleRoute s _ -> Just s
        _                 -> Nothing

      selected = Maybe.map ((==) n) (rs m.route)
        |> Maybe.withDefault False
  in
    li [ classList [("selected", selected)] ] 
       [ a [ href (routeToUrl <| ScheduleRoute n Nothing)] [ text n ] ]
   
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

-- Meets

meets : Model -> List (Html Msg)
meets m =
  case m.schedule of
    NoSchedule -> [ h2 [] [ text "no schedule selected"] ]
    FetchingSchedule -> [ h2 [] [ text "..."] ]
    FetchedSchedule s -> scheduleView s m
    ErroredSchedule e -> [ h2 [] [ text e ] ]

scheduleView : Schedule -> Model -> List (Html Msg) 
scheduleView r m =
  case (m.timezone, m.route) of
    (Just tz, ScheduleRoute _ fgmt) -> 
      [ h2 [] [ text r.host] 
      , ul [] (List.map (meetView fgmt tz m.hasCreds) r.meets) 
      ]
    _                               -> [ p [] [ text "No timezone." ] ]

meetView : (Maybe String) -> T.Zone -> Bool -> Meet -> Html Msg
meetView fgmt z creds m = 
  li [ class "meetitem" ]
    [ a [ href (addMeetFragment m.name)
        , id m.name
        ] 
      [ dl [ class "meetdata"] 
        [ dt [] [ text "name" ]
        , dd [] [ text m.name ]
        , dt [] [ text "start"]
        , dd [] [ text (startTime z m.time) ]
        , dt [] [ text "duration"]
        , dd [] [ text (duration m.dur) ]
        ]
      , bookView fgmt creds m
      ]
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

bookView : Maybe String -> Bool -> Meet -> Html Msg
bookView fgmt hasCreds m =
  let
    focus =  classList [ ("focus", hasMeetfocus m.name fgmt)]   
  in
    if hasCreds
      then button [ focus, onClick (Book m) ] [ text "Book" ] 
      else button [ focus, onClick NeedsCreds ] [ text "Log in to book this meet." ]

hasMeetfocus : String -> Maybe String -> Bool
hasMeetfocus id fgmt =
  Maybe.map ((==) id) fgmt 
  |> Maybe.withDefault False