#!/bin/bash

build() {
  mkdir -p debug
  cp src/index.html debug/
  cp src/main.css debug/
  elm make src/Main.elm --debug --output=debug/main.debug.js
}

export -f build

serve debug &

sleep 1

open "$(pbpaste)"

find src | entr -s build