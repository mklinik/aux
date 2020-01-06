#!/bin/bash

WORK_TIME=25
BREAK_TIME=5

case "$1" in
  "")
    echo "usage: pomodoro.sh <session>"
    echo "where <session> is work, break, or a number in minutes"
    exit
    ;;
  w*)
    x=$WORK_TIME;
    ;;
  b*)
    x=$BREAK_TIME;
    ;;
  *)
    x="$1"
    ;;
esac

while [ $x -gt 0 ]
do
  echo $x mins to go
  sleep 1m
  x=$(( $x - 1 ))
done
date
echo -e '\a'
notify-send -t 0 "Session complete" "$(date)"
