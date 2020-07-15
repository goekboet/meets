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
import Bookings
import Weekpointer exposing (Weekpointer)


-- Appointment

type alias Flags =
  { antiCsrf: SS.AntiCsrfToken
  , username: Maybe SS.Username
  , publicBrokerUrl : String
  , weekpointer : Weekpointer
  }

type alias Model =
  { key : Nav.Key
  , sessionState : SS.Model
  , page: Maybe Page
  , hostsModel : Hosts.Model
  , timesModel : Times.Model
  , bookingsModel : Bookings.Model
  }

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | HostsMessage Hosts.Msg
  | TimesMessage Times.Msg
  | BookingsMessage Bookings.Msg

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
      signedIn = SS.isSignedIn session
      page = Page.fromUrl url
  in
  ( { key = key
    , sessionState = session
    , page = page
    , hostsModel = Hosts.init flags.publicBrokerUrl
    , timesModel = Times.init signedIn flags.publicBrokerUrl flags.weekpointer
    , bookingsModel = Bookings.init
    }
  , case (signedIn, page) of
    (_, Just HostsPage) -> Hosts.fetchHosts HostsMessage flags.publicBrokerUrl Nothing Nothing
    (_, Just (TimesPage h)) -> Times.fetchTimes TimesMessage h flags.publicBrokerUrl flags.weekpointer  
    (True, Just BookingsPage) -> Bookings.listBookings BookingsMessage
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

            isLoggedIn = SS.isSignedIn model.sessionState
          in
            ( { model | page = nRoute }
            , case (isLoggedIn, nRoute) of
              (_, Just HostsPage) -> Hosts.fetchHosts HostsMessage publicUrl Nothing Nothing
              (_, Just (TimesPage h)) -> Times.fetchTimes TimesMessage h publicUrl model.timesModel.weekpointer  
              (True, Just BookingsPage) -> Bookings.listBookings BookingsMessage
              _ -> Cmd.none
            )

        HostsMessage hs -> 
          let
              (hModel, cs) = Hosts.update HostsMessage hs model.hostsModel
          in
          ( { model | hostsModel = hModel }, cs)

        TimesMessage hs ->
          let
              (hModel, cs) = Times.update TimesMessage hs model.timesModel
          in
          ( { model | timesModel = hModel }, cs)

        BookingsMessage hs ->
          let
              (hModel, cs) = Bookings.update BookingsMessage hs model.bookingsModel
          in
          ( { model | bookingsModel = hModel }, cs)
         

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m = 
  case m.page of
     Just HomePage      -> Sub.none
     Just BookingsPage  -> Bookings.subscribe BookingsMessage
     Just HostsPage     -> Sub.none
     Just (TimesPage h) -> Times.subscribe TimesMessage h
     _                  -> Sub.none

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
          , a [ BookingsPage |> Page.toUrl |> href ]
              [ Html.h3 [] [ text "Bookings" ]
              , p [] [ text "Any bookings you made shows up here." ]
              ]
          ]
        ]
  in
    if SS.isSignedIn m.sessionState
    then loggedIn
    else notLoggedIn

loginRequired : Model -> List (Html Msg)    
loginRequired m =
  [ Html.h2 [] [ Html.text "Login required"]
  , Html.p [] 
    [ Html.text "This page requires you to "
    , SS.formLink m.sessionState (Page.loginUrl HomePage) (Html.text "login") 
    , Html.text " in order to do anything useful."
    ]
  ]
pageView : Model -> List (Html Msg)
pageView m =
  let
    signinLink p = SS.formLink m.sessionState (Page.loginUrl p) (Html.text "login")     
  in
  case (SS.isSignedIn m.sessionState, m.page) of
    (_,Just HomePage) -> indexView m
    (False,Just BookingsPage) -> loginRequired m
    (True, Just BookingsPage) -> Bookings.view BookingsMessage m.bookingsModel
    (_,Just HostsPage) -> Hosts.view HostsMessage m.hostsModel
    (s,Just (TimesPage h)) -> Times.view TimesMessage h (TimesPage h |> signinLink) s m.timesModel
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



