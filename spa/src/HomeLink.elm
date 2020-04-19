module HomeLink exposing (homelink, homeLinkStyle)

import Html exposing (Attribute, Html, a, text)
import Html.Attributes exposing (href, class)

homeLinkStyle : List (Attribute msg)
homeLinkStyle =
    [ class "pt-1em"
    , class "heavy-bkg"
    ]

homelink : List (Html msg)
homelink =
    [ a
        [ href "/"
        , class "alt-txt-col"
        , class "large-text"
        ]
        [ text "meets" ]
    ]