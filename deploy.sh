#!/bin/bash

sitename=elm-youtube

rm -rf ~/Sites/$sitename
./build.sh
cp -r ./docs ~/Sites
mv ~/Sites/docs ~/Sites/$sitename
open http://localhost/$sitename