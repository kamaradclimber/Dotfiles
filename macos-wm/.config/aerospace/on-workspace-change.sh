#!/bin/bash
/opt/homebrew/bin/hs -c "aerospaceMarkDirty()" >/dev/null 2>&1
sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE="$AEROSPACE_FOCUSED_WORKSPACE" &
