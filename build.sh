#!/bin/bash

mkdir -p docs
cp src/index.html docs/
sed -i '' 's/debug/build/' docs/index.html # changes main.debug.js to main.build.js
cp src/main.css docs/

elm make src/Main.elm --optimize --output=docs/main.build.js
