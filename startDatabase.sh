#!/bin/bash

port=5432

if [ "$1" != "" ]; then
    port=$1
fi

sudo docker build -t iw_db .
sudo docker run -p $port:5432 -it iw_db

