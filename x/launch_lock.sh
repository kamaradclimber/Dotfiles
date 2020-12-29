#!/usr/bin/bash

# This is used by awesome to start the lock service to allow to unlock when I plug my phone in.
# for some reason, using this command in directly with awful.spawn does not seem to work but
# launching a script that contain this command does.

sudo /usr/bin/systemctl start --no-block i3lock.service
