#!/usr/bin/bash

source=$(pamixer --list-sources | grep '"GoMic' | awk '{print $1}')
pamixer --source $source -t
pamixer --source $source --get-mute
pamixer --source $source --get-volume-human
