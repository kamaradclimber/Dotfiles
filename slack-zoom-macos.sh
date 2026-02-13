#!/bin/bash

# Detect if currently in a Zoom meeting on macOS and notify peon-status.sh
# Uses process-based detection (no accessibility permissions needed)
# Detection: CptHost process only runs during active meetings

set -e

DOTFILES="$HOME/.dotfiles"
STATE_DIR="$HOME/.cache/zoom-detector"
STATE_FILE="$STATE_DIR/in_meeting"

mkdir -p "$STATE_DIR"

# Check if Zoom is running and if we're in a meeting
# CptHost is the Zoom meeting media engine and only runs during active meetings
if pgrep -x "zoom.us" > /dev/null && pgrep -f "CptHost" > /dev/null; then
    in_meeting="true"
else
    in_meeting="false"
fi

# Read previous state
if [[ -f "$STATE_FILE" ]]; then
    was_in_meeting=$(cat "$STATE_FILE")
else
    was_in_meeting="false"
fi

# Handle state transitions
if [[ "$in_meeting" == "true" ]] && [[ "$was_in_meeting" == "false" ]]; then
    # Just entered a meeting
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Entered Zoom meeting"
    "$DOTFILES/peon-status.sh" in-meeting
    echo "true" > "$STATE_FILE"
elif [[ "$in_meeting" == "false" ]] && [[ "$was_in_meeting" == "true" ]]; then
    # Just exited a meeting
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Exited Zoom meeting"
    "$DOTFILES/peon-status.sh" not-in-meeting
    echo "false" > "$STATE_FILE"
fi
