#!/bin/sh

elm-make src/Main.elm --output=build/index.html

ln -sf ../resources/world.jpg build/world.jpg

if [ ! -e build/airports.json ]; then
    echo "Downloading airport data..."
    python3 get_airports.py > build/airports.json
fi

