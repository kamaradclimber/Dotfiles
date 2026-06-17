#!/bin/bash

# Detect if currently in a Zoom meeting on macOS and notify peon-status.sh
# Uses process-based detection (no accessibility permissions needed)
# Detection: CptHost process only runs during active meetings

set -e

DOTFILES="$HOME/.dotfiles"
STATE_DIR="$HOME/.cache/zoom-detector"
STATE_FILE="$STATE_DIR/in_meeting"
OFFICE_SSID="wi-fido"

mkdir -p "$STATE_DIR"

# Check if Zoom is running and if we're in a meeting
# CptHost is the Zoom meeting media engine and only runs during active meetings
if pgrep -x "zoom.us" > /dev/null && pgrep -f "CptHost" > /dev/null; then
    in_meeting="true"
else
    in_meeting="false"
fi

# Check if connected to office WiFi
current_ssid=$(networksetup -getairportnetwork en0 2>/dev/null | sed 's/Current Wi-Fi Network: //')
if [[ "$current_ssid" == "$OFFICE_SSID" ]]; then
    in_office="true"
else
    in_office="false"
fi

# Mute sound when in office and not in a meeting
if [[ "$in_office" == "true" ]] && [[ "$in_meeting" == "false" ]]; then
    osascript -e 'set volume output muted true' > /dev/null 2>&1 || true
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
