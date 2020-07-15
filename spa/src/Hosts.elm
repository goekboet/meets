module Hosts exposing (Model, Msg, view, init, update, fetchHosts)

import Html exposing (Html)
import Html.Attributes as Attr
import FontAwesome as FA
import Page exposing (Page(..))
import Http exposing (Error)
import Html.Events as Event
import Url.Builder as UrlB
import Json.Decode as Json exposing ( Decoder )

type alias Host =
    { name : String
    , handle : String
    }

decodeHost : Decoder Host
decodeHost =
    Json.map2 Host
    (Json.field "name" Json.string)
    (Json.field "handle" Json.string)

type Status 
    = Received
    | Pending
    | Error

type alias HostData =
    { status : Status
    , data : List Host
    , page : Maybe Int
    , filter : Maybe String
    }

initHostData = 
    { status = Received
    , data = []
    , page = Nothing
    , filter = Nothing
    }

refetch : Maybe String -> HostData
refetch f = 
    { status = Pending
    , data = []
    , page = Nothing
    , filter = f
    }

nextPage : HostData -> HostData
nextPage hd = { hd | status = Pending }



append : HostData -> HostData
append hd = { hd | status = Pending }
    

addHosts : (List Host) -> HostData -> HostData
addHosts hs hd = 
    let
        nextHd = 
            { status = Received
            , data = hd.data ++ hs
            , filter = hd.filter
            , page = hd.page 
            }
    in
    
    case (List.length hs, hd.page) of
    (100, Just page) -> { nextHd | page = Just (page + 1) }
    (100, Nothing)   -> { nextHd | page = Just 1 }
    _                -> { nextHd | page = Nothing }

errorHosts : HostData -> HostData
errorHosts hd = { hd | status = Error }

type alias Filter = Maybe String

type alias Model =
    { publicApiBaseUrl : String
    , hostData : HostData
    , filter : Filter
    }

init : String -> Model
init baseUrl = 
    { publicApiBaseUrl = baseUrl
    , hostData = initHostData
    , filter = Nothing
    }

type Msg 
    = RefetchHosts
    | HostsReceived (Result Error (List Host))
    | FilterChange String
    | FetchNextPage 

fetchHosts : (Msg -> msg) -> String -> (Maybe Int) -> (Maybe String) -> Cmd msg
fetchHosts toAppMsg baseUrl p notBefore =
    let
        pQuery = p |> Maybe.map ( UrlB.string "p" << String.fromInt )
        nbfQyery = notBefore |> Maybe.map (UrlB.string "notBeforeName") 
        
        url = 
          UrlB.crossOrigin baseUrl
          [ "hosts" ] 
          (List.filterMap (\x -> x) [ pQuery, nbfQyery ])
            
    in
        Http.get 
        { url = url
        , expect = Http.expectJson (toAppMsg << HostsReceived) (Json.list decodeHost) 
        }

fetchNextPage : (Msg -> msg) -> String -> HostData -> Cmd msg
fetchNextPage toApp baseUrl hd =
    fetchHosts toApp baseUrl hd.page hd.filter

    
update : (Msg -> msg) -> Msg -> Model -> (Model, Cmd msg)
update toApp msg model =
    case msg of
        RefetchHosts -> 
            ( { model | hostData = refetch model.filter }
            , fetchHosts toApp model.publicApiBaseUrl Nothing model.filter 
            )
        HostsReceived (Ok hs) ->  
            ( { model | hostData = addHosts hs model.hostData }
            , Cmd.none)

        HostsReceived (Err _) ->  
            ( { model | hostData = errorHosts model.hostData }
            , Cmd.none)

        FilterChange f -> 
            ( { model | filter = Just f }, Cmd.none )

        FetchNextPage ->
            ( { model | hostData = nextPage model.hostData }
            , fetchNextPage toApp model.publicApiBaseUrl model.hostData
            )

searchButton : (Msg -> msg) -> HostData -> Html msg
searchButton toApp hd =
    case hd.status of
    Pending -> Html.button 
               [ Attr.disabled True] 
               [ Html.text "Search" ]
    _       -> Html.button 
               [ Event.onClick (toApp RefetchHosts) ] 
               [ Html.text "Search"]

hostView : (Msg -> msg) -> HostData -> Html msg
hostView toApp hd =
    let
        item h = 
            Html.li [] 
            [ FA.fas_fa_user_circle
            , Html.label [] [ Html.text h.name ]
            , Html.a [ Page.toUrl (TimesPage h.handle) |> Attr.href ] [ Html.text "times" ]]

        nextLink =
            case hd.page of
            Just p -> 
                [ Html.li 
                  [ Attr.class "hostsNextPage" ] 
                  [ Html.button [ Event.onClick (toApp FetchNextPage) ] [ Html.text "more" ]
                  ]
                ]
            _ -> []

        
    in
        List.map item hd.data ++ nextLink |> Html.ul [ Attr.class "hostsList" ]

errorMessage : HostData -> Html msg
errorMessage hd =
    case hd.status of
       Error -> Html.p [ Attr.class "hostsError" ]
                [ FA.fas_fa_exclamation_circle 
                , Html.label [] [ Html.text "There was an error fetching hosts. Please try again later. "
                ]
                ]
       _     -> Html.text ""

view : (Msg -> msg) -> Model -> List (Html msg)
view toApp m =
    [ Html.h2 [] [ Html.text "Hosts" ] 
    , Html.p [] [ Html.text "List hosts that publish times with us. The filter is lexicographic, i.e. it filters out hosts with names that come before the supplied string in an alphabetically sorted list. Each host has a link to a listing of their times." ]
    , errorMessage m.hostData
    , Html.span [ Attr.class "hostsFilter"]
      [ Html.label [] [ Html.text "Filter" ]
      , Html.input 
        [ Attr.type_ "text" 
        , Event.onInput (toApp << FilterChange)
        ] 
        []
      , searchButton toApp m.hostData
      ]
    , Html.hr [ Attr.class "line" ] []
    , hostView toApp m.hostData
    ]