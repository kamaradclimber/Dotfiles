#!/bin/bash

# Manage peon status based on external events (like Zoom meetings)
# Usage: peon-status.sh <in-meeting|not-in-meeting>

set -e

PEON="$HOME/.claude/hooks/peon-ping/peon.sh"
STATUS_DIR="$HOME/.cache/peon-status"
SAVED_STATUS_FILE="$STATUS_DIR/saved_status"

mkdir -p "$STATUS_DIR"

zoom_status="$1"

if [[ "$zoom_status" == "in-meeting" ]]; then
    # Save current peon status and pause
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Saving peon status and pausing"
    bash "$PEON" --status > "$SAVED_STATUS_FILE" 2>&1 || true
    bash "$PEON" --pause

elif [[ "$zoom_status" == "not-in-meeting" ]]; then
    # Restore peon status if we have one saved
    if [[ -f "$SAVED_STATUS_FILE" ]]; then
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Restoring peon status"
        saved_status=$(cat "$SAVED_STATUS_FILE")

        if echo "$saved_status" | grep -q "paused"; then
            # Was already paused, keep it paused
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Peon was already paused before meeting"
        else
            # Resume peon
            bash "$PEON" --resume || bash "$PEON" --start || true
        fi

        rm -f "$SAVED_STATUS_FILE"
    fi

else
    echo "Usage: $0 <in-meeting|not-in-meeting>"
    exit 1
fi
