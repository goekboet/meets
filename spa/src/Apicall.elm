module Apicall exposing (
    toMsg,
    loadHosts, 
    callUnbook, 
    callBook, 
    callBookings, 
    callTimes, 
    ApiCall(..), 
    ApiBaseUrl 
    )

import Model exposing (Msg(..), Appointment, decodeHosts, HostId, Window, decodeAppointments, decodeAppointment, encodeAppointment, UnixTs)
import Route exposing (Route(..))
import Url.Builder as UrlB
import Http exposing (Error)

type ApiCall a
    = Uncalled
    | Pending
    | Response a
    | Error String

type alias ApiBaseUrl =
    String

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

toMsg : Error -> String
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
