module FontAwesome exposing 
    ( fas_fa_chevron_circle_down
    , fas_fa_arrow_alt_circle_left
    , fas_fa_arrow_alt_circle_right
    , fas_fa_arrow_alt_circle_up
    , fas_fa_arrow_alt_circle_down
    , fas_fa_check_circle
    , far_fa_check_circle
    , fas_fa_sign_out_alt
    , fas_fa_exclamation_triangle
    , fas_fa_user_circle
    , fas_fa_sync_alt_control
    , fas_minus_circle
    , fas_fa_sync_alt_rolls
    , fas_fa_sync_alt
    , fas_fa_exclamation_circle)

import Html exposing (Html, i) 
import Html.Events exposing ( onClick )
import Html.Attributes exposing ( class, classList)

fas_fa_chevron_circle_down : Html msg
fas_fa_chevron_circle_down = i [ class "fas", class "fa-chevron-circle-down"] []

fas_fa_arrow_alt_circle_left : Html msg
fas_fa_arrow_alt_circle_left = i [ class "fas", class "fa-arrow-alt-circle-left"] []

fas_fa_arrow_alt_circle_right : Html msg
fas_fa_arrow_alt_circle_right = i [ class "fas", class "fa-arrow-alt-circle-right"] []

fas_fa_arrow_alt_circle_up : Html msg
fas_fa_arrow_alt_circle_up = i [ class "fas", class "fa-arrow-alt-circle-up"] []

fas_fa_arrow_alt_circle_down : Html msg
fas_fa_arrow_alt_circle_down = i [ class "fas", class "fa-arrow-alt-circle-down"] []

fas_fa_check_circle : Html msg
fas_fa_check_circle = i [ class "fas", class "fa-check-circle" ] []

far_fa_check_circle : Html msg
far_fa_check_circle = i [ class "far", class "fa-check-circle" ] []

fas_fa_sign_out_alt : Html msg
fas_fa_sign_out_alt = i [class "fas", class "fa-sign-out-alt" ] []

fas_fa_exclamation_triangle : Html msg
fas_fa_exclamation_triangle = i [ class "fas", class "fa-exclamation-triangle" ] []

fas_fa_exclamation_circle : Html msg
fas_fa_exclamation_circle = i [ class "fas", class "fa-exclamation-circle" ] []

fas_fa_user_circle : Html msg
fas_fa_user_circle = i [ class "fas", class "fa-user-circle" ] []

fas_minus_circle : Html msg
fas_minus_circle = i [ class "fas", class "fa-minus-circle" ] []

fas_fa_sync_alt_control : Bool -> msg -> Html msg
fas_fa_sync_alt_control syncing trigger = 
    i 
          [ Html.Events.onClick trigger
          , classList 
            [ ("fas", True) 
            , ("fa-sync-alt", True)
            , ("rolls", syncing)
            ]
          ] 
          []

fas_fa_sync_alt_rolls : Html msg
fas_fa_sync_alt_rolls = i [ class "fas fa-sync-alt rolls"] []
fas_fa_sync_alt : Html msg
fas_fa_sync_alt = i [ class "fas fa-sync-alt"] []
  