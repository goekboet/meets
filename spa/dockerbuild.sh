#! /bin/bash

echo "Building with image $1"
docker run --mount \
    "type=bind,source=/Users/erikgook/Repos/byappt/apps/meets/server/wwwroot/,destination=/home/spa/dist" \
    $1
