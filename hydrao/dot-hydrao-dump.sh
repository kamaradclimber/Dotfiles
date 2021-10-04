#!/usr/bin/bash

HOME=/home/grego

cd $HOME/hydrao-dump
source my_env
python receiver.py $HYDRAO_MAC_ADDRESS
