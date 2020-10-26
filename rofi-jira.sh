#!/bin/bash

newfile=$(mktemp jiradump.XXXX)
file=/tmp/jira-dump.txt
ssh g_seux@churchill.criteois.lan "cat .jira-dump.txt" > $newfile

if [ -s $newfile ]; then
  mv $newfile $file
fi

echo $file

rofi-generic --input-files $file
