#!/bin/bash

# TODO: at some point this should be written in a scripting language such as ruby

set -e

KODI_HOST=kodi

# PI:Analogue => normal speakers
# PI:HDMI => through projector
# PI:Both => through projector & speakers
# ALSA:default => projector
# ALSA:pulse => bluetooth or nothing




data="{\"jsonrpc\":\"2.0\",\"method\":\"Settings.GetSettingValue\", \"params\":{\"setting\":\"audiooutput.audiodevice\"},\"id\":1}'))"

echo "Current:"
curl -s --data-binary "$data" -H 'content-type: application/json;' http://$KODI_HOST:8080/jsonrpc
echo

if [ -n "$1" ]; then
  data="{\"jsonrpc\":\"2.0\",\"method\":\"Settings.SetSettingValue\", \"params\":{\"value\": \"$1\", \"setting\":\"audiooutput.audiodevice\"},\"id\":1}'))"
  echo "Setting $1"
  curl -s --data-binary "$data" -H 'content-type: application/json;' http://$KODI_HOST:8080/jsonrpc
  echo
fi
