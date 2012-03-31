#!/bin/bash

# embed an image into the meta-data of an audio file

# supports mp3 and ogg audio files, and jpeg images

# Prerequisites:
#  eyeD3 (for mp3 support)
#  vorbis-tools (for ogg support)

# Example:
#  $ ls
#  01-rancid-indestructible.ogg  cover.jpg
#  $ embed.sh 01-rancid-indestructible.ogg

# Notes for n900 users:
#
# - For OGG files, the newest version of the n900 media player ignores
# cover.jpg and folder.jpg, instead it needs cover art embedded in the ogg
# file. For mp3 files, cover.jpg works fine. This is probably because the
# tracker-extractor for ogg is a different program (you have to install an
# additional package) and they forgot to add support for cover.jpg.
#
# - The n900 media player only needs the cover embedded in the first file of an
# album to correctly display the album art for all tracks of the album
#
# - Album art for mp3 files works with cover.jpg and embedded (I have tried the
# tags FRONT_COVER and OTHER, both work). Other filenames work, too (I have
# tried FRONT_COVER.jpg) It's just those darn mp3 files that 7digital
# distributes that don't work. The cached files in .mediaartlocal and
# ~/.cache/media-art are broken for some reason. file says: 'data'. My guess is
# the file comment that 7digital includes, has backslashes.
#
# Hence, for mp3 files from 7digital, do the following BEFORE copying the files
# to the n900:
#  $ eyeD3 -i . <first-audio-file>
#  $ eyeD3 --remove-images *.mp3
#
# Or, if you already have broken images in your cache, do the following on your PC:
#  # use sshfs to mount your n900 home directory
#  $ file n900/.cache/media-art | grep data # find all garbled files in the global cache
#  # delete those files
#  $ cd n900/MyDocs/<problematic-album>
#  $ rm -r .mediaartlocal # delete local cache
#  $ eyeD3 -i . <first-audio-file> # extract FRONT_COVER.jpg, which the n900 recognizes
#  $ eyeD3 --remove-images *.mp3
#  # count to three, monty python style (to give the indexer time to notice
#  that something has changed), and restart the media player


function usage()
{
  echo "usage: $0 [image-file] <audio-file>"
  echo "if image-file is not given, default to cover.jpg"
}

if [ $# -eq 1 ]
then
  AUDIO_FILE=$1
  IMAGE_FILE="cover.jpg"
else if [ $# -eq 2 ]
then
  IMAGE_FILE=$1
  AUDIO_FILE=$2
else
  usage
  exit 1
fi
fi

if [ ! -f $AUDIO_FILE ]
then
  echo "audio file '$AUDIO_FILE' not found."
  exit 1
fi

if [ ! -f $IMAGE_FILE ]
then
  echo "image file '$IMAGE_FILE' not found."
  exit 1
fi

case $AUDIO_FILE in
*.mp3)
    eyeD3 --add-image=$IMAGE_FILE:OTHER $AUDIO_FILE
    ;;
*.ogg)
    (echo -n coverart=; base64 -w 0 $IMAGE_FILE; echo; echo coverartmime=image/jpeg) | vorbiscomment -a $AUDIO_FILE
    ;;
**)
    echo "$AUDIO_FILE: unsupported audio file format"
    exit 1
    ;;
esac
