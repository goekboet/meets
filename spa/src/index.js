import { week } from './dates.js'

let appelement = document.getElementById("entrypoint");

let usernameData = appelement.getAttribute("data-username")

let flags = 
    { antiCsrf: appelement.getAttribute("data-anticsrf")
    , username: usernameData === "" ? null : usernameData
    , publicBrokerUrl: appelement.getAttribute("data-publicbrokerurl")
    , weekpointer: week(new Date())
    }

let app = Elm.Main.init({ flags: flags });

app.ports.moveWeekpointer.subscribe(ts => {
    let d = ts === null
        ? new Date()
        : new Date(ts * 1000)

    app.ports.newWeekpointer.send(week(d))
})