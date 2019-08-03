import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Url
import Url.Builder

-- MAIN


main : Program Flags Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , hasCreds: Bool
  }

type alias Flags =
  { hasCreds : Bool
  }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url flags.hasCreds, Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | NeedsCreds
  | Book


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    NeedsCreds ->
      ( model, Nav.load "/home/login?~%2F")

    Book ->
      ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

credsStatus : Model -> String
credsStatus m = if m.hasCreds then "Authenticated" else "Unauthenticated"

view : Model -> Browser.Document Msg
view model =
  { title = "meets - public client"
  , body =
      [ div [] 
        [ p [] [(text <| credsStatus model)]
        , booklink model
        ]]
  }

booklink : Model -> Html Msg
booklink m = if m.hasCreds
  then
    button [ onClick NeedsCreds ] [ text "book" ]
  else
    button [ onClick NeedsCreds] [ text "book"]

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]