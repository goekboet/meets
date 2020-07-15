module Bookings exposing (Model, Msg, init, update, subscribe, view, listBookings)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as Json exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Http exposing (Error)
import Url.Builder as UrlB
import FontAwesome as FA

type alias Booking =
    { host : String
    , name : String
    , start : Int
    , dur : Int
    }

decodeBooking : Decoder Booking
decodeBooking =
    Json.map4 Booking
    (Json.field "host" Json.string)
    (Json.field "name" Json.string)
    (Json.field "start" Json.int)
    (Json.field "dur" Json.int)

encodeBooking : Booking -> Value
encodeBooking { host, name, start, dur } =
  Encode.object
    [ ("host", Encode.string host)
    , ("name", Encode.string name)
    , ("start", Encode.int start)
    , ("dur", Encode.int dur)
    ]

type Status 
    = Received
    | UnBooked
    | Pending
    | Error

type alias BookingsItem = (Status, Booking)

type alias BookingsData = (Status, List BookingsItem)

refresh : List Booking -> BookingsData
refresh bs = (Received, bs |> List.map (Tuple.pair Received))

setPending : BookingsData -> BookingsData
setPending (_, bs) = (Pending, bs)

setError : BookingsData -> BookingsData
setError (_, bs) = (Error, bs)

setItemStatus : Int -> Status ->  BookingsData -> BookingsData
setItemStatus id newS (status, bs) =
    let
        f (s, b) = 
            if b.start == id
            then (newS, b)
            else (s, b)
    in
        (status, List.map f bs)

setPendingUnbook : Int -> BookingsData -> BookingsData
setPendingUnbook id bs = setItemStatus id Pending bs

setUnbooked : Int -> BookingsData -> BookingsData
setUnbooked id bs = setItemStatus id UnBooked bs

setUnbookedError : Int -> BookingsData -> BookingsData
setUnbookedError id bs = setItemStatus id Error bs

dismissBooking : Int -> BookingsData -> BookingsData
dismissBooking id (status, bd) = (status, List.filter (\(_, x) -> x.start /= id) bd)
    

type alias Model = BookingsData

init : Model
init = (Pending, [])

type Msg 
    = RefreshBookings
    | ReceivedBookings (Result Error (List Booking))
    | Unbook Booking
    | Unbooked Int (Result Error ())
    | Dismiss Int

listBookings : (Msg -> msg) -> Cmd msg
listBookings toApp =
    let
        url = UrlB.absolute [ "api", "bookings" ] []
    in
        Http.get
        { url = url
        , expect = Http.expectJson (toApp << ReceivedBookings) (Json.list decodeBooking)}

unBook : (Msg -> msg) -> Int -> Cmd msg 
unBook toApp id =
    let
        url = UrlB.absolute [ "api", "bookings", String.fromInt id ] []
    in
        Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever (toApp << Unbooked id)
        , timeout = Nothing
        , tracker = Nothing
        }

update : (Msg -> msg) -> Msg -> Model -> (Model, Cmd msg) 
update toApp msg m =
    case msg of
    RefreshBookings -> 
        ( setPending m
        , listBookings toApp)

    ReceivedBookings (Ok bs) -> 
        ( refresh bs 
        , Cmd.none)

    ReceivedBookings (Err _) -> 
        ( setError m
        , Cmd.none)

    Unbook b -> 
        ( setPendingUnbook b.start m
        , unBook toApp b.start)

    Unbooked id (Ok _) -> 
        ( setUnbooked id m
        , Cmd.none)

    Unbooked id (Err _) -> 
        ( setUnbookedError id m
        , Cmd.none)

    Dismiss id ->
        ( dismissBooking id m
        , Cmd.none)

subscribe : (Msg -> msg) -> Sub msg 
subscribe _ = Sub.none

refreshBookings : (Msg -> msg) -> BookingsData -> Html msg
refreshBookings toApp (s, _) =
    Html.span [ Attr.class "bookingsRefresh" ]
    [ Html.label [] [ Html.text "Bookings:" ]
    , Html.button 
      [ Event.onClick (toApp RefreshBookings)
      , Attr.disabled (s == Pending)] 
      [ Html.text "refresh" ]]

bookingsList : (Msg -> msg) -> BookingsData -> Html msg
bookingsList toApp (_, bs) =
    let
        listItem (s, b) =
            case s of
                Received ->
                    Html.li []
                    [ FA.far_fa_calendar_check
                    , Html.label [] [ Html.text b.name ]
                    , Html.button 
                      [ Unbook b |> toApp |> Event.onClick ] 
                      [ Html.text "unbook"]
                    ]

                Pending ->
                    Html.li []
                    [ FA.fas_fa_sync_alt_rolls
                    , Html.label [] [ Html.text b.name ]
                    , Html.button [ Attr.disabled True ] [ Html.text "unbook"]
                    ]

                UnBooked ->
                    Html.li []
                    [ FA.far_fa_calendar_times
                    , Html.label [] [ Html.text ("Unbooked: " ++ b.name) ]
                    , Html.button 
                      [ Dismiss b.start |> toApp |> Event.onClick ] 
                      [ Html.text "dismiss"]
                    ]
                    
                Error ->
                    Html.li []
                    [ FA.fas_fa_exclamation_circle
                    , Html.label [] [ Html.text ("Error unbooking: " ++ b.name) ]
                    , Html.button 
                      [ Dismiss b.start |> toApp |> Event.onClick ] 
                      [ Html.text "dismiss"]
                    ]
    in
        List.map listItem bs |> Html.ul [ Attr.class "bookingsList"]

view : (Msg -> msg) -> Model -> List (Html msg)
view toApp m = 
    [ Html.h2 [] [ Html.text "Bookings" ]
    , Html.p [] [ Html.text "You can review and unbook bookings here. When time comes you can go to your appointement."]
    , refreshBookings toApp m
    , Html.hr [ Attr.class "line" ] []
    , bookingsList toApp m
    ]