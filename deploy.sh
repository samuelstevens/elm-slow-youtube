#!/bin/bash

sitename=elm-youtube

rm -rf ~/Sites/$sitename
./build.sh
cp -r ./build ~/Sites
mv ~/Sites/build ~/Sites/$sitename
open http://localhost/$sitename