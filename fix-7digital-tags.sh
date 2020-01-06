#!/bin/bash

if [ $# -lt 1 ]
then
  echo "usage: $0 <file.mp3>"
  echo "Extracts embedded image from given song; removes all images; adds image to all songs."
  exit 0
fi

eyeD3 -i . $1
eyeD3 --remove-images *.mp3
eyeD3 --add-image=FRONT_COVER.jpeg:FRONT_COVER *.mp3
