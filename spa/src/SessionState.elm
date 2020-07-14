module SessionState exposing 
    ( Username
    , AntiCsrfToken
    , Model
    , init
    , isSignedIn
    , sessionEnded
    , antiCsrfRefreshed
    , formLink
    )

import Html exposing (Html, input, button)
import Html.Attributes exposing (class, action, method, type_, name, value, disabled)

type alias Username = String

type alias AntiCsrfToken = String

type alias Model = 
    { signedIn : Maybe Username
    , antiCsrf : Maybe AntiCsrfToken
    }

init : Maybe Username -> Maybe AntiCsrfToken -> Model
init signIn antiCsrf =
    { signedIn = signIn, antiCsrf = antiCsrf }

sessionEnded : Model
sessionEnded = 
    { signedIn = Nothing, antiCsrf = Nothing }

antiCsrfRefreshed : Model -> String -> Model
antiCsrfRefreshed m token =
    { signedIn = m.signedIn, antiCsrf = Just token }


isSignedIn : Model -> Bool
isSignedIn m =
    case m.signedIn of
        Just _ -> True
        _      -> False

formLink : Model -> String -> Html msg -> Html msg
formLink model url label =
    case model.antiCsrf of
    Just t -> 
        Html.form
            [ action url
            , method "post"
            , class "formlink"
            ]
            [ button
                [ type_ "submit"
                ]
                [ label ]
            , input
                [ type_ "hidden"
                , name "__RequestVerificationToken"
                , value t
                ]
                []
            ]
    _ ->
        Html.form
            [ action url
            , method "post"
            , class "formlink"
            ]
            [ button
                [ type_ "submit"
                , disabled True
                ]
                [ label ]
            ]