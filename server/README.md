# Meets

The purpose of this repo is to provide a view for the public into availiable meets. If a member of the public is authenticated with ids then meets enables this user to post bookings for meets and list the users bookings.

## Docker

Naming convention: ègo/meets:{semver}

To open a shell: `docker run -it --rm --entrypoint=/bin/bash ego/meets:{semver}`

The middlewre that handles authorization with ids needs to
1. Call the discovery endpoint to configure itself and fetch the keys. Since its running in a docker container it cannot use localhost.
2. Redirect the client to ids. For the client ids is reachable from localhost.
To work around this run `ipconfig getifaddr en0` this is the ip of the docker host and use this.

