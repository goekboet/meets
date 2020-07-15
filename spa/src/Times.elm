port module Times exposing (Model, Weekpointer, Msg, init, update, subscribe, view, fetchTimes)

import Html exposing (Html)
import Html.Attributes as Attr
import FontAwesome as FA
import Html.Events as Event
import Http exposing (Error)
import Url.Builder as UrlB
import Json.Decode as Json exposing (Decoder)
import Json.Encode as Encode exposing (Value)

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

encodeTime : Time -> Value
encodeTime { host, name, start, dur } =
  Encode.object
    [ ("host", Encode.string host)
    , ("name", Encode.string name)
    , ("start", Encode.int start)
    , ("dur", Encode.int dur)
    ]

type alias Week = 
    { name : String
    , ts: Int
    , isNow : Bool
    }

type alias Weekpointer =
    { current: Week
    , previous: Week
    , next : Week
    }

getWeekWindow : Weekpointer -> (Int, Int)
getWeekWindow { current, previous, next } =
    (current.ts, next.ts)

type Status 
    = Received
    | Booked
    | Pending
    | Error

type alias TimeItem = (Status, Time)

type alias TimeData =
    { status : Status
    , times : List TimeItem
    }

setPending : TimeData -> TimeData
setPending td = { td | status = Pending }

newTimes : List Time -> TimeData
newTimes ts = { status = Received, times = List.map (Tuple.pair Received) ts }

errorTimes : TimeData -> TimeData
errorTimes td = { td | status = Error }



updateStatus : Int -> Status -> TimeData -> TimeData
updateStatus id status td =
    let
        f (s, t) = 
            if t.start == id
            then (status, t)
            else (s, t)

    in
        { td | times = List.map f td.times }

pendingBooking : Int -> TimeData -> TimeData
pendingBooking id td = updateStatus id Pending td

successfulBooking : Int -> TimeData -> TimeData   
successfulBooking id td = updateStatus id Booked td

failedBooking : Int -> TimeData -> TimeData
failedBooking id td = updateStatus id Error td

initData : TimeData
initData = 
    { status = Received
    , times = []
    }

type alias Model =
    { baseUrl : String
    , loggedIn : Bool
    , data : TimeData
    , weekpointer : Weekpointer
    }

init : Bool -> String -> Weekpointer -> Model
init l url wp =
    { baseUrl = url
    , loggedIn = l
    , data = initData
    , weekpointer = wp
    }

port moveWeekpointer : Maybe Int -> Cmd msg
port newWeekpointer : (Weekpointer -> msg) -> Sub msg

type Msg 
    = RefreshTimes String
    | TimesReceived (Result Error (List Time))
    | Move (Maybe Int)
    | New String Weekpointer
    | Book Time
    | BookReceived Int (Result Error ())

fetchTimes : (Msg -> msg) -> String -> String -> Weekpointer -> Cmd msg
fetchTimes toApp host baseUrl wp =
    let
        (from, to) = getWeekWindow wp
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

book : (Msg -> msg) -> Time -> Cmd msg
book toApp t =
    let
        url = UrlB.absolute [ "api", "bookings" ] []
    in
    Http.post
    { url = url
    , body = encodeTime t |> Http.jsonBody
    , expect = Http.expectWhatever (toApp << BookReceived t.start)
    }

update : (Msg -> msg) -> Msg -> Model -> (Model, Cmd msg)
update toApp msg model =
    case msg of
      RefreshTimes h ->
        ( { model | data = setPending model.data}
        , fetchTimes toApp h model.baseUrl model.weekpointer)  

      TimesReceived (Ok ts) -> 
        ( { model | data =  newTimes ts }
        , Cmd.none)  

      TimesReceived (Err _) -> ({ model | data = errorTimes model.data }, Cmd.none) 

      Move ts -> ( model, moveWeekpointer ts)  

      New h wp ->
        ( { model | weekpointer = wp }
        , fetchTimes toApp h model.baseUrl wp
        )

      Book t ->
        ( { model | data = pendingBooking t.start model.data }
        , book toApp t
        )

      BookReceived id (Ok _) ->
        ( { model | data = successfulBooking id model.data }
        , Cmd.none
        )

      BookReceived id (Err _) ->
        ( { model | data = failedBooking id model.data }
        , Cmd.none
        )

subscribe : (Msg -> msg) -> String -> Sub msg 
subscribe toAppmsg host =
    newWeekpointer (toAppmsg << New host)

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

weekPointerControls : (Msg -> msg) -> String -> Weekpointer -> Html msg
weekPointerControls toMsg h { current, previous, next } =
    Html.span [ Attr.class "timesWeekpointer"]
      [ Html.button 
        [ Event.onClick (Move Nothing |> toMsg) ] 
        [ FA.fas_fa_chevron_circle_down ]
      , Html.button 
        [ Event.onClick (Just previous.ts |> Move |> toMsg) ] 
        [ FA.fas_fa_arrow_alt_circle_left ]
      , Html.button 
        [ Event.onClick (Just next.ts |> Move |> toMsg) ] 
        [ FA.fas_fa_arrow_alt_circle_right ]
      , Html.label [] [ Html.text current.name ]
      ]

timesList : (Msg -> msg) -> Model -> Html msg    
timesList toApp m =
    let
        listItem (s, t) =
            case s of
            Received -> 
                Html.li [] 
                [ FA.far_fa_clock
                , Html.label [] [ Html.text t.name ]
                , Html.button 
                  [ Event.onClick (Book t |> toApp) 
                  , Attr.disabled (not m.loggedIn) ] 
                  [ Html.text "book" ]
                ]

            Booked -> 
                Html.li [] 
                    [ FA.fas_fa_check_circle
                    , Html.label [] [ Html.text t.name ]
                    , Html.button 
                      [ ] 
                      [ Html.text "go to bookings" ]
                    ]

            Pending -> 
                Html.li [] 
                    [ FA.fas_fa_sync_alt_rolls
                    , Html.label [] [ Html.text t.name ]
                    , Html.button 
                      [ Attr.disabled True ] 
                      [ Html.text "book" ]
                    ]

            Error -> 
                Html.li [] 
                    [ FA.fas_fa_exclamation_circle
                    , Html.label [] [ Html.text ("Error booking time " ++ t.name ++ ".") ]
                    , Html.button 
                      [ Event.onClick (Book t |> toApp)] 
                      [ Html.text "book" ]
                    ]
            
    in
        List.map listItem m.data.times |> Html.ul [ Attr.class "timesList"]

view : (Msg -> msg) -> String -> Html msg -> Bool -> Model -> List (Html msg)
view toApp host signInLink isSignedIn m =
    [ Html.h2 [] [ Html.text "Times" ] 
    , errorMessage m.data 
    , Html.p [] [ Html.text "Browse times this host has published. Step between the current, previous and next weeks." ]
    , weekPointerControls toApp host m.weekpointer
    , singInReminder signInLink isSignedIn
    , refreshTimes toApp host
    , Html.hr [ Attr.class "line" ] []
    , timesList toApp m
    ]
