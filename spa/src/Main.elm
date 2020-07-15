module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (Html, h1, p, div, text, a)
import FontAwesome as FA
import Html.Events exposing (onClick)
import Html.Attributes as Attr exposing (class, href)
import SessionState as SS
import Page exposing (Page(..))
import Hosts
import Times


-- Appointment

type alias Flags =
  { antiCsrf: SS.AntiCsrfToken
  , username: Maybe SS.Username
  , publicBrokerUrl : String
  }

type alias Model =
  { key : Nav.Key
  , sessionState : SS.Model
  , page: Maybe Page
  , hostsModel : Hosts.Model
  , hostModel : Times.Model
  }

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | HostsMessage Hosts.Msg
  | HostMessage Times.Msg

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



-- Init


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
      session = SS.init flags.username (Just flags.antiCsrf)
      page = Page.fromUrl url
  in
  ( { key = key
    , sessionState = session
    , page = page
    , hostsModel = Hosts.init flags.publicBrokerUrl
    , hostModel = Times.init (SS.isSignedIn session) flags.publicBrokerUrl}
  , case page of
    Just HostsPage -> Hosts.fetchHosts HostsMessage flags.publicBrokerUrl Nothing Nothing
    Just (TimesPage h) -> Times.fetchTimes HostMessage h flags.publicBrokerUrl 0 0  
    _ -> Cmd.none
  )



-- UPDATE


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
          let
            nRoute = Page.fromUrl url
            publicUrl = model.hostsModel.publicApiBaseUrl
          in
            ( { model | page = nRoute }
            , case nRoute of
              Just HostsPage -> Hosts.fetchHosts HostsMessage publicUrl Nothing Nothing
              Just (TimesPage h) -> Times.fetchTimes HostMessage h publicUrl 0 0  
              _ -> Cmd.none
            )

        HostsMessage hs -> 
          let
              (hModel, cs) = Hosts.update HostsMessage hs model.hostsModel
          in
          ( { model | hostsModel = hModel }, cs)

        HostMessage hs ->
          let
              (hModel, cs) = Times.update HostMessage hs model.hostModel
          in
          ( { model | hostModel = hModel }, cs)
         




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

homelink : SS.Model -> Maybe Page -> Html msg
homelink ss page =
    div [ class "content"
        , class "heavy" 
        , class "home"
        ] 
        [ h1 [] 
          [ a
            [ Page.toUrl HomePage |> href
            ]
            [ text "Meets" ]
          ]
        , if SS.isSignedIn ss
          then SS.formLink ss (Maybe.withDefault HomePage page |> Page.logoutUrl ) FA.fas_fa_sign_out_alt
          else text ""
        ]

indexView : Model -> List (Html Msg)
indexView m =
  let
      notLoggedIn = 
        [ Html.h2 [] [ text "Welcome" ]
        , Html.p []
          [ Html.text "Meets lets you browse and book times that folks publish with us. To book you need to "
          , SS.formLink m.sessionState (Page.loginUrl HomePage) (Html.text "login") 
          , Html.text " so we know who you are." ]
        , Html.span [ class "indexLink" ]
          [ a [ HostsPage |> Page.toUrl |> href ]
            [ Html.h3 [] [ text "Hosts" ]
            , Html.p [] [ text "List and search hosts that publish times with us." ]
            ]
          , a [ class "homeNotLoggedIn" ]
              [ Html.h3 [] [ text "Bookings" ]
              , p [] [ text "If you are logged in you can browse your booked times here." ]
              ]
          ]
        ]

      loggedIn =
        [ Html.h2 [] [ text "Welcome" ]
        , Html.span [ class "indexLink" ]
          [ a [ HostsPage |> Page.toUrl |> href ]
            [ Html.h3 [] [ text "Hosts" ]
            , Html.p [] [ text "List and search hosts that publish times with us." ]
            ]
          , a [ ]
              [ Html.h3 [] [ text "Bookings" ]
              , p [] [ text "Any bookings you made shows up here." ]
              ]
          ]
        ]
  in
    if SS.isSignedIn m.sessionState
    then loggedIn
    else notLoggedIn
    
pageView : Model -> List (Html Msg)
pageView m =
  let
    signinLink p = SS.formLink m.sessionState (Page.loginUrl p) (Html.text "login")     
  in
  case m.page of
    Just HomePage -> indexView m
    Just BookingsPage -> []
    Just HostsPage -> Hosts.view HostsMessage m.hostsModel
    Just (TimesPage h) -> Times.view HostMessage h (TimesPage h |> signinLink) (SS.isSignedIn m.sessionState) m.hostModel
    _ -> []




view : Model -> Browser.Document Msg
view model =
    { title = "meets - public client"
    , body =
        [ div 
        [ class "root-view" ] 
        [ homelink model.sessionState model.page
        , div 
          [ class "content"
          , class "light" 
          , class "app-view" 
          ] 
          (pageView model)
        ] 
      ]
    }



