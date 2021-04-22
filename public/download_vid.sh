#!/bin/sh

VID=$1
START=$2
DURATION=$3
OUT=$4

URLS=$(/usr/local/bin/youtube-dl --youtube-skip-dash-manifest -g "${VID}")
VIDEO=$(echo "${URLS}" | head -n 1)
AUDIO=$(echo "${URLS}" | tail -n 1)
ffmpeg -ss "${START}" -i "${VIDEO}" -ss "${START}" -i "${AUDIO}" -map 0:v -map 1:a -t "${DURATION}" -c:v libx264 -c:a aac "${OUT}"
