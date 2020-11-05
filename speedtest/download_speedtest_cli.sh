#!/bin/sh

START_DIR=$DIR

wget https://bintray.com/ookla/download/download_file?file_path=ookla-speedtest-1.0.0-x86_64-linux.tgz -O /app/speedtest.tgz
cd /app
tar -xvf speedtest.tgz

cd "$START_DIR"
