#!/usr/bin/env bash

set -euo pipefail

PORT=5000
DIR=`esy status | jq -r .rootBuildPath`
EXE="$DIR/default/bin/server.exe"

function clear_process {
  port=$1
  echo "clearning port $1"
  kill -SIGTERM `lsof -t -i ":${port}"` || true
  kill -9 `lsof -t -i ":${port}"` || true
}

function cleanup {
  ps aux | grep $EXE | awk '{print $2}' | xargs kill || true
  ps aux | grep $EXE | awk '{print $2}' | xargs kill -9 || true

  clear_process $((PORT))
  clear_process $((PORT+1))
  clear_process $((PORT+2))
  clear_process $((PORT+3))
  clear_process $((PORT+4))
}

cleanup

$EXE $((PORT+1)) &
$EXE $((PORT+2)) &
$EXE $((PORT+3)) &
$EXE $((PORT+4)) &
sleep 1
nginx -c `pwd`/nginx.conf


sleep infinity

trap cleanup EXIT SIGINT SIGTERM
