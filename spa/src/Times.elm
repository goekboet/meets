module Times exposing (Model, Msg, init, update, view, fetchTimes)

import Html exposing (Html)
import Html.Attributes as Attr
import FontAwesome as FA
import Html.Events as Event
import Http exposing (Error)
import Url.Builder as UrlB
import Json.Decode as Json exposing (Decoder)

type alias Time =
    { host : String
    , name : String
    , start : Int
    , dur : Int
    }

decodeTime : Decoder Time
decodeTime =
    Json.map4 Time
    (Json.field "host" Json.string)
    (Json.field "name" Json.string)
    (Json.field "start" Json.int)
    (Json.field "dur" Json.int)

type Status 
    = Received
    | Pending
    | Error

type alias TimeData =
    { status : Status
    , times : List Time
    }

setPending : TimeData -> TimeData
setPending td = { td | status = Pending }

newTimes : List Time -> TimeData
newTimes ts = { status = Received, times = ts }

errorTimes : TimeData -> TimeData
errorTimes td = { td | status = Error }

initData : TimeData
initData = 
    { status = Received
    , times = []
    }

type alias Model =
    { baseUrl : String
    , loggedIn : Bool
    , data : TimeData
    }

init : Bool -> String -> Model
init l url =
    { baseUrl = url
    , loggedIn = l
    , data = initData
    }

type Msg 
    = RefreshTimes String
    | TimesReceived (Result Error (List Time))

fetchTimes : (Msg -> msg) -> String -> String -> Int -> Int -> Cmd msg
fetchTimes toApp host baseUrl from to =
    let
        url = 
            UrlB.crossOrigin baseUrl
            [ "hosts", host, "times" ]
            [ String.fromInt from |> UrlB.string "from"
            , String.fromInt to |> UrlB.string "to"
            ]
    in
        Http.get
            { url = url
            , expect = Http.expectJson (toApp << TimesReceived) (Json.list decodeTime)
            }

update : (Msg -> msg) -> Msg -> Model -> (Model, Cmd msg)
update toApp msg model =
    case msg of
       RefreshTimes h -> 
        ( { model | data = setPending model.data}
        , fetchTimes toApp model.baseUrl h 0 0)
       TimesReceived (Ok ts) -> ({ model | data = newTimes ts }, Cmd.none)
       TimesReceived (Err e) -> ({ model | data = errorTimes model.data }, Cmd.none)

singInReminder : Html msg -> Bool -> Html msg
singInReminder signInLink signedIn =
    if signedIn
    then Html.text ""
    else 
        Html.p [] 
        [ Html.text "In order to book any times you need to "
        , signInLink
        ]

refreshTimes : (Msg -> msg) -> String -> Html msg
refreshTimes toApp host =
    Html.span [ Attr.class "timesRefresh" ]
    [ Html.label [] [ Html.text "Times:" ]
    , Html.button [ Event.onClick (toApp (RefreshTimes host))] [ Html.text "refresh" ]]

errorMessage : TimeData -> Html msg
errorMessage td =
    case td.status of
       Error -> Html.p [ Attr.class "hostsError" ]
                [ FA.fas_fa_exclamation_circle 
                , Html.label [] [ Html.text "There was an error fetching hosts. Please try again later. "
                ]
                ]
       _     -> Html.text ""

view : (Msg -> msg) -> String -> Html msg -> Bool -> Model -> List (Html msg)
view toApp host signInLink isSignedIn m =
    [ Html.h2 [] [ Html.text "Times" ] 
    , errorMessage m.data 
    , Html.p [] [ Html.text "Browse times this host has published. Step between the current, previous and next weeks." ]
    , singInReminder signInLink isSignedIn
    , refreshTimes toApp host
    ]
