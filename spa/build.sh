#! /bin/bash

echo "Transpiling to js."
elm make src/Main.elm --optimize --output=../server/wwwroot/main.js
echo "Output $(wc -c ../server/wwwroot/main.js) bytes."

echo "Minifying elm..."
uglifyjs ../server/wwwroot/main.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=../server/wwwroot/main.min.js
rm ../server/wwwroot/main.js
echo "Output $(wc -c ../server/wwwroot/main.min.js) bytes."

echo "Bundling javascript.."
browserify dates.js -o ../server/wwwroot/bundle.js
echo "Output $(wc -c ../server/wwwroot/bundle.js) bytes."

echo "Minifying javacsript..."
uglifyjs ../server/wwwroot/bundle.js --output=../server/wwwroot/bundle.min.js
rm ../server/wwwroot/bundle.js
echo "Output $(wc -c ../server/wwwroot/bundle.min.js) bytes."

cp style.css ../server/wwwroot/style.css