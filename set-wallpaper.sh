#!/usr/bin/bash

source=https://www.reddit.com/r/EarthPorn+ImaginaryLandscapes+ImaginaryTechnology+futureporn.json

curl -s -L -H 'User-Agent: custom-timer' "$source" | jq '.data.children[] |.data.url' | head -100 | sort -R | xargs feh --bg-fill
