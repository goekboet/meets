import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Url
import Url.Builder as UrlB
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Time as T
import Task exposing ( perform )

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



-- MODEL

type Schedules =
  Fetching
  | Fetched (List String)
  | Errored String

type ScheduleResult =
  NoSchedule
  | FetchingSchedule
  | FetchedSchedule Schedule
  | ErroredSchedule String

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , hasCreds: Bool
  , timezone: Maybe T.Zone
  , schedules: Schedules
  , selectedSchedule: Maybe String
  , schedule: ScheduleResult
  }

type alias Flags =
  { hasCreds : Bool
  }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { key = key
    , url = url
    , hasCreds = flags.hasCreds
    , timezone = Nothing
    , schedules = Fetching
    , selectedSchedule = Nothing
    , schedule = NoSchedule 
    }
  , Cmd.batch 
    [ callSchedules
    , perform AgentZone T.here
    ]
  )

-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | AgentZone T.Zone
  | FetchedSchedules (Result Http.Error (List String))
  | SelectedSchedule String
  | FetchedMeets (Result Http.Error Schedule)
  | NeedsCreds
  | Book


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    AgentZone z ->
      ( { model | timezone = (Just z) }
      , Cmd.none
      )

    FetchedSchedules (Err e) ->
      ( recordScheduleError model e
      , Cmd.none
      )

    FetchedSchedules (Ok ss) ->
      ( receiveSchedules model ss
      , Cmd.none
      )

    SelectedSchedule s ->
      ( { model 
        | selectedSchedule = Just s 
        , schedule = FetchingSchedule
        }
      , callMeets s )

    FetchedMeets (Err e) ->
      ( recordError model e, Cmd.none )

    FetchedMeets (Ok res) ->
      ( receiveSchedule model res, Cmd.none )
    
    NeedsCreds ->
      ( model, Nav.load "/login?~%2F")

    Book ->
      ( model, Cmd.none )

callSchedules : Cmd Msg
callSchedules =
  Http.get
    { url = schedulesUrl
    , expect = Http.expectJson FetchedSchedules decodeSchedules }

receiveSchedules : Model -> (List String) -> Model
receiveSchedules m ss =
  { m | schedules = Fetched ss }

recordScheduleError : Model -> Http.Error -> Model
recordScheduleError m err =
  case err of
    Http.BadUrl s -> { m | schedules = Errored <| ("Invalid url: " ++ s) }
    Http.Timeout  -> { m | schedules = Errored <| ("Request timed out.") }
    Http.NetworkError -> { m | schedules = Errored <| ("Network error.") }
    Http.BadStatus s -> { m | schedules = Errored <| ("Bad status: " ++ String.fromInt s)}
    Http.BadBody s -> { m | schedules = Errored <| ("Bad body: " ++ s) }

meetsUrl : String -> String
meetsUrl n = UrlB.absolute [ "api", "schedules", n ] []

callMeets : String -> Cmd Msg
callMeets s =
  Http.get
    { url = meetsUrl s
    , expect = Http.expectJson FetchedMeets decodeSchedule
    }

receiveSchedule : Model -> Schedule -> Model
receiveSchedule m s =
  { m | schedule = FetchedSchedule s }

recordError : Model -> Http.Error -> Model
recordError m err =
  case err of
    Http.BadUrl s -> { m | schedule = ErroredSchedule <| ("Invalid url: " ++ s) }
    Http.Timeout  -> { m | schedule = ErroredSchedule <| ("Request timed out.") }
    Http.NetworkError -> { m | schedule = ErroredSchedule <| ("Network error.") }
    Http.BadStatus s -> { m | schedule = ErroredSchedule <| ("Bad status: " ++ String.fromInt s)}
    Http.BadBody s -> { m | schedule = ErroredSchedule <| ("Bad body: " ++ s) }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

credsStatus : Bool -> String
credsStatus s = if s then "Authenticated" else "Unauthenticated"

view : Model -> Browser.Document Msg
view model =
  { title = "meets - public client"
  , body = layout 
      (header model) 
      (schedules model) 
      (meets model)
  }

layout : List (Html Msg) -> List (Html Msg) -> List (Html Msg) -> List (Html Msg)
layout head side content =
  [ div [ id "layout"] 
        [ div [ id "head" ] head 
        , div [ id "side"] side
        , div [ id "content"] content
        ]
  ]

header : Model -> List (Html Msg)
header m = 
  [ h1 [] [ text "meets"]
  , i [] [ text <| credsStatus m.hasCreds ]
  ]

schedule : Model -> String -> Html Msg
schedule m n =
  let
      selected = Maybe.map ((==) n) m.selectedSchedule
        |> Maybe.withDefault False
  in
    li [ classList [("selected", selected)] 
       , onClick (SelectedSchedule n)] 
       [ text n ]
   

schedules : Model -> List (Html Msg)
schedules m =
  case m.schedules of
    Fetching   -> [ i [] [ text "fetching" ]]
    Fetched ss -> [ h2 [] [ text "schedules" ]
                  , ul [ class "schedules-list" ] (List.map (schedule m) ss) ]
    Errored e  -> [ i [] [text e] ]

decodeSchedules : Decoder (List String)
decodeSchedules =  Decode.list (Decode.field "name" Decode.string)

schedulesUrl : String
schedulesUrl = UrlB.absolute [ "api", "schedules" ] []

type alias Schedule =
  { host : String
  , meets : List Meet
  }

type alias Meet =
  { name : String
  , time: Int
  , dur: Int
  }

decodeMeets : Decoder (List Meet)
decodeMeets = 
  Decode.list <| Decode.map3 Meet 
    (Decode.field "name" Decode.string)
    (Decode.field "time" Decode.int)
    (Decode.field "dur" Decode.int)


decodeSchedule : Decoder Schedule
decodeSchedule =
  Decode.map2 Schedule 
    (Decode.field "host" Decode.string)
    (Decode.field "meets" decodeMeets)

meets : Model -> List (Html Msg)
meets m =
  case m.schedule of
    NoSchedule -> [ h2 [] [ text "no schedule selected"] ]
    FetchingSchedule -> [ h2 [] [ text "..."] ]
    FetchedSchedule s -> scheduleView s m.timezone 
    ErroredSchedule e -> [ h2 [] [ text e ] ]

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

meetView : T.Zone -> Meet -> Html Msg
meetView z m = li []
      [ dl [] 
        [ dt [] [ text "name" ]
        , dd [] [ text m.name ]
        , dt [] [ text "start"]
        , dd [] [ text (startTime z m.time) ]
        , dt [] [ text "duration"]
        , dd [] [ text (duration m.dur) ]
        ]
      ]

scheduleView : Schedule -> (Maybe T.Zone) -> List (Html Msg) 
scheduleView r agentz =
  case agentz of
    Just tz -> List.concat
      [ [ h2 [] [ text r.host] ]
      , [ ul [] (List.map (meetView tz) r.meets) ]
      ]
    Nothing -> [p [] [ text "No timezone." ] ]
  
  
  

-- booklink : Model -> Html Msg
-- booklink m = if m.hasCreds
--   then
--     button [ onClick NeedsCreds ] [ text "book" ]
--   else
--     button [ onClick NeedsCreds] [ text "book"]

-- viewLink : String -> Html msg
-- viewLink path =
--   li [] [ a [ href path ] [ text path ] ]