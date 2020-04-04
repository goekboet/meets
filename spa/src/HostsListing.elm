module HostsListing exposing 
    ( HostsData
    , receiveData
    , isEmpty
    , requestData
    , recordFilterQuery
    , recordHostFilterInput
    , hostsDataView
    , hostsFilter
    , hostsFilterStyle
    )

import Apicall exposing (ApiCall(..))
import Model exposing (Msg(..), Host)
import Route exposing (Route(..))
import Html exposing (Html, Attribute, div, button, text, i, h4, p, a, dl, dd, dt, label, input)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)


type alias HostsData =
    { calls : List (ApiCall (List Host))
    , nextPage : Int
    , filter : String
    }

isEmpty : HostsData -> Bool
isEmpty h = h.nextPage == 0  

recordFilterQuery : HostsData -> HostsData
recordFilterQuery hs =
    { calls = [ Pending ], nextPage = 0, filter = hs.filter }

requestData : HostsData -> HostsData
requestData hs =
    case hs.calls of
        Uncalled :: cs -> { hs | calls = Pending :: cs } 
        _ -> hs  

receiveData : ApiCall (List Host) -> HostsData -> HostsData
receiveData call hs =
    case call of
            Response h -> 
                if List.length h == 100 then
                    { calls = Uncalled :: call :: List.drop 1 hs.calls
                    , nextPage = hs.nextPage + 1
                    , filter = hs.filter 
                    }
                else
                    { calls = call :: List.drop 1 hs.calls
                    , nextPage = 0
                    , filter = hs.filter
                    }
            _ ->    { calls = call :: List.drop 1 hs.calls
                    , nextPage = 0
                    , filter = hs.filter
                    }

recordHostFilterInput : HostsData -> String -> HostsData
recordHostFilterInput hs f =
    { hs | filter = f }



getFilterParameter : Route -> (Maybe String)
getFilterParameter r =
    case r of
        HostsRoute f -> f
        _            -> Nothing
hostListingStyle : List (Attribute Msg)
hostListingStyle =
    [ class "pt-b-2em"
    , class "light-bkg"
    ]


hostListing : Host -> Html Msg
hostListing h =
    div hostListingStyle
        [ dl []
            [ dt [] [ text "handle" ]
            , dd [ class "hosthandle" ] [ text h.handle ]
            , dt [] [ text "name" ]
            , dd [ class "hostname" ] [ text h.name ]
            ]
        ]

hostsCall : (Maybe String) -> Int -> ApiCall (List Host)  -> List (Html Msg)
hostsCall filter nextP call =
    case call of
        Uncalled -> 
            [ div [ class "hostlistAction" ] 
                [ button 
                    [ onClick (FetchHosts filter (Just nextP))
                    , class "centered"
                    , class "light-button" ] 
                    [ text "Load more..."] 
                ] 
            ]

        Pending ->
            [ div [ class "hostlistAction" ]
                [ div 
                    [ id "loading"
                    , class "centered" ] 
                    [] 
                ] 
            ]

        Response hs ->
            List.map hostListing hs

        Error ms ->
            [ div [ class "error"] 
            [ i [ class "fas fa-exclamation-triangle" ] []
            , h4 [] [ text "An error occured."] 
            , p [] 
                [ text "You can always "
                , a [ onClick (FetchHosts Nothing (Just nextP)) ] [ text "try again."] ]
            ] 
            ]

hostsDataView : Route -> HostsData -> List (Html Msg)
hostsDataView r data =
    [ div 
        [ id "scrollArea", class "hostlist" ] 
        (List.concatMap (hostsCall (getFilterParameter r) data.nextPage) (List.reverse data.calls)) ]

hostsFilterStyle : List (Attribute Msg)
hostsFilterStyle = 
    [ class "pt-b-2em"
    , class "light-bkg"
    , class "light-sep"
    , class "hostsfilter"]

hostsFilter : List (Html Msg)
hostsFilter =  
    [ label [ Html.Attributes.for "hostsfilterinput" ] [ text "Lexicographically greater than:" ] 
    , input 
        [ Html.Attributes.type_ "text"
        , id "hostsfilterinput"
        , Html.Attributes.name "hostsfilter"
        , Html.Events.onInput HostsFilterInput
        ] 
        []
    , button [ onClick HostsFilterQuery ] [ i [ class "fas fa-sync"] [] ]
    ]
