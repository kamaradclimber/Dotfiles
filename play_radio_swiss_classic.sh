#!/bin/bash

# TODO: at some point this should be written in a scripting language such as ruby

set -e

KODI_HOST=kodi

echo "Will play Radio swiss classic"

data='{"id":0,"jsonrpc":"2.0","method":"Player.Open","params":{"item":{"file":"plugin://plugin.audio.radio_de/station/3464"}}}'

curl -s --fail --show-error --data-binary "$data" -H 'content-type: application/json;' http://$KODI_HOST:8080/jsonrpc
