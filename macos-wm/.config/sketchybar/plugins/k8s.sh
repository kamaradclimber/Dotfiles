#!/bin/sh


ctx="$(kubectx -c)"


ICON="k8s:"

LABEL="$ctx"

sketchybar --set $NAME icon="$ICON" label="$LABEL"
