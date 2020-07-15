module Page exposing 
    ( Page(..)
    , fromUrl
    , toUrl
    , loginUrl
    , logoutUrl
    )

import Url exposing (Url)
import Url.Parser as UrlP exposing (Parser, (</>))
import Url.Builder as UrlB

type Page
    = HomePage 
    | BookingsPage 
    | HostsPage
    | TimesPage String 


fromUrl : Url -> Maybe Page
fromUrl url = UrlP.parse route url

route : Parser (Page -> a) a
route =
    UrlP.oneOf
        [ UrlP.map HomePage UrlP.top 
        , UrlP.map BookingsPage (UrlP.s "bookings")
        , UrlP.map HostsPage (UrlP.s "hosts")
        , UrlP.map TimesPage (UrlP.s "hosts" </> UrlP.string)
        ]

toUrl : Page -> String
toUrl r = 
    case r of
        HomePage -> UrlB.absolute [] [] 
        BookingsPage -> UrlB.absolute [ "bookings" ] []
        HostsPage -> UrlB.absolute [ "hosts" ] []
        TimesPage handle -> UrlB.absolute [ "hosts", handle ] []
            
loginUrl : Page -> String
loginUrl r =
    UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (toUrl r) ]

logoutUrl : Page -> String
logoutUrl r = UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (toUrl r) ] 
    
    
