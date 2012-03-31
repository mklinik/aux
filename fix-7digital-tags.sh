#!/bin/bash

eyeD3 -i . 01*
eyeD3 --remove-images *.mp3
eyeD3 --add-image=FRONT_COVER.jpeg:FRONT_COVER *.mp3
