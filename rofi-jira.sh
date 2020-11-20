#!/bin/bash

file=/run/user/1000/jira-dump.txt

if [[ ! -e $file ]]; then
  echo "jira dump has not ran yet" >> $file
fi

rofi-generic --input-files $file
