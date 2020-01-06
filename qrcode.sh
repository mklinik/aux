#!/bin/bash

qrencode -o - "$1" | display -
