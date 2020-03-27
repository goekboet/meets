module Model exposing (UnixTs, Appointment, Msg(..), Host, WeekPointer, Week, Window, HostId, decodeHosts, decodeHost, decodeAppointments, decodeAppointment, encodeAppointment)

import Browser exposing (UrlRequest)
import Url exposing (Url)
import Debounce as Debounce exposing (Debounce)
import Http as Http exposing (Error)
import Json.Decode as Decode exposing (Error, Decoder)
import Json.Encode as Encode exposing (Value)

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | WeekpointerDebounced Debounce.Msg
    | FetchHosts (Maybe String) (Maybe Int)
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

type alias Host =
    { handle : String
    , name : String
    }

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

decodeHost : Decoder Host
decodeHost =
    Decode.map2 Host
        (Decode.field "handle" Decode.string)
        (Decode.field "name" Decode.string)

decodeHosts : Decoder (List Host)
decodeHosts =
    Decode.list decodeHost

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

type alias Week =
    String

type alias Start =
    Int

type alias End =
    Int
type alias Window = ( Start, End )

type alias WeekPointer =
    { prev : Week
    , curr : Week
    , next : Week
    , window : Window
    }

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