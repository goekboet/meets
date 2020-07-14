let appelement = document.getElementById("entrypoint");

let usernameData = appelement.getAttribute("data-username")

let flags = 
    { antiCsrf: appelement.getAttribute("data-anticsrf")
    , username: usernameData === "" ? null : usernameData
    , publicBrokerUrl: appelement.getAttribute("data-publicbrokerurl")
    }

let app = Elm.Main.init({ flags: flags });