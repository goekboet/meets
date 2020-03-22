#! /bin/bash

set -e
DEST=${1:-dist}

SECONDS=0
echo "building to $DEST"
echo "Transpiling to js."
elm make src/Main.elm --optimize --output="${DEST}/main.js"
echo "Output $(wc -c ${DEST}/main.js) bytes"



SECONDS=0
echo "Minifying elm..."
uglifyjs "${DEST}/main.js" --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output="${DEST}/main.js"
echo "Output $(wc -c ${DEST}/main.js) bytes."

SECONDS=0
echo "Bundling javascript.."
browserify dates.js -o "${DEST}/bundle.js"
echo "Output $(wc -c ${DEST}/bundle.js) bytes."

SECONDS=0
echo "Minifying javacsript..."
uglifyjs "${DEST}/bundle.js" --output="${DEST}/bundle.js"
echo "Output $(wc -c ${DEST}/bundle.js) bytes."

cp style.css "${DEST}/style.css"
cp favicon.ico $DEST

echo "Finished build of spa."
echo $(ls $DEST)