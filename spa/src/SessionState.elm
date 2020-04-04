module SessionState exposing (SessionState, init, recordStaleness, isSignedIn, sessionstateView)

import Html exposing (Html, Attribute, p, i, input, text, b, button, a)
import Route exposing (Route, logoutUrl)
import Html.Attributes exposing (class, action, method, type_, name, value)
import Html.Events exposing (onClick)
import Model exposing (Msg(..))

type SessionState
    = Fresh String
    | Stale
    | None

init : Maybe String -> SessionState
init name = 
    Maybe.map Fresh name
    |> Maybe.withDefault None

recordStaleness : SessionState
recordStaleness = Stale

isSignedIn : SessionState -> Bool
isSignedIn s =
    case s of
        Fresh _ ->
            True

        _ ->
            False
sessionStateText : List (Attribute Msg)
sessionStateText =
    [ class "alt-txt-col"
    , class "small-text"
    ]

logoutTrigger : Route -> String -> Html Msg
logoutTrigger route csrf =
    Html.form
        [ action (logoutUrl route)
        , method "post"
        , class "inline"
        , class "logoutTrigger"
        ]
        [ button
            [ type_ "submit"
            ]
            [ i [ class "fas", class "fa-sign-out-alt", class "alt-txt-col" ] [] ]
        , input
            [ type_ "hidden"
            , name "__RequestVerificationToken"
            , value csrf
            ]
            []
        ]

sessionstateView : Route -> String -> SessionState -> List (Html Msg)
sessionstateView r csrf s =
    case s of
        Fresh name ->
            [ p
                sessionStateText
                [ text "You are logged in as " ]
            , b sessionStateText [ text name ]
            , text "."
            , logoutTrigger r csrf
            ]

        Stale ->
            [ p
                [ class "alt-txt-col"
                , class "small-text"
                ]
                [ text "Your session has expired. You need to "
                , a [ onClick NeedsCreds ] [ text "log in" ]
                , text " again."
                ]
            ]

        None ->
            [ p
                [ class "alt-txt-col"
                , class "small-text"
                ]
                [ text "You can browse publicly listed hosts and times anonymously. However, to claim a time you need to prove your identity by "
                , a [ onClick NeedsCreds ] [ text "logging in" ]
                , text "."
                ]
            ]