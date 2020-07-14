#!/bin/bash

build_debug() {
    echo "Compiling elm."
    elm make src/Main.elm \
        --debug \
        --output $1/app.js

    echo "Bundling js dependencies."
    browserify src/index.js \
        -o $1/index.js \
        -t [ babelify --presets [ @babel/preset-env ] ]  
}

build() {
    echo "Compile elm."
    elm make src/Main.elm \
        --optimize \
        --output $1/app.js 

    echo "Minify elm."
    uglifyjs $1/app.js -c "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" \
        | uglifyjs --mangle -o $1/app.js

    echo "Bundle js-dependencies."
    browserify src/index.js \
        -o $1/index.js \
        -t [ babelify --presets [ @babel/preset-env ] ] 

    echo "Minify js-dependecies."
    uglifyjs $1/index.js --compress --mangle \
        -o $1/index.js
}

dist="dist"
dest="prod"
i=1;
for arg in "$@" 
do
    if [ $i -eq 1 -a $arg == "-debug" ];
    then dest="debug"
    elif [ $i -eq 1 -a $arg != "-debug" ] 
    then dist=$arg
    fi
    if [ $i -eq 2 -a $arg == "-debug" ]
    then dest="debug"
    fi
    i=$((i + 1));
done
echo "dist: $dist"
echo "dest: $dest"

mkdir -p $dist
rm -r $dist/* 2>/dev/null

if [[ $dest = "debug" ]];
then build_debug $dist
else build $dist
fi

echo "Concatenate stylesheets."
cat $(find src -type f -name "*.css") > $dist/style.css
echo "Copy static assets."
cp src/favicon.ico $dist/favicon.ico

ls -lh $dist