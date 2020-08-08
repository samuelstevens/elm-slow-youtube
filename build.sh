#!/bin/bash

mkdir -p build
cp src/index.html build/
sed -i '' 's/debug/build/' build/index.html # changes main.debug.js to main.build.js
cp src/main.css build/

elm make src/Main.elm --optimize --output=build/main.build.js
