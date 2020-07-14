let appelement = document.getElementById("entrypoint");

let usernameData = appelement.getAttribute("data-username")
let flags = 
    { antiCsrf: appelement.getAttribute("data-anticsrf")
    , username: usernameData === "" ? null : usernameData
    }
console.log(flags);
let app = Elm.Main.init({ flags: flags });