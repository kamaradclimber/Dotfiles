#!/bin/bash

# TODO: at some point this should be written in a scripting language such as ruby

set -e

KODI_HOST=kodi

playlist[0]="7aXwTPQQ1_U" # bolero ravel
playlist[1]="4Tr0otuiQuU" # beethoven moonlight sonata
playlist[2]="YQh2UCA_cOY" # borodine danses polovtsiennes

music=${playlist[$RANDOM % ${#playlist[@]} ]}

echo "Will play $music"

data="{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"method\":\"Player.Open\",\"params\":{\"item\":{\"file\":\"plugin://plugin.video.youtube/play/?video_id=$music\"}}}"

curl -s --fail --show-error --data-binary "$data" -H 'content-type: application/json;' http://$KODI_HOST:8080/jsonrpc
