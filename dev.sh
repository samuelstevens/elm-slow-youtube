#!/bin/bash

serve dev &

sleep 1

open "$(pbpaste)"

find src | entr elm make src/Main.elm --debug --output=dev/main.debug.js