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
# - The newest version of the n900 media player ignores cover.jpg and
# folder.jpg, instead it needs cover art embedded in the audio file.
#
# - The n900 media player only needs the cover embedded in the first file of an
# album to correctly display the album art for all tracks of the album
#
# - The n900 media player needs the cover art for mp3 files embedd in the tag
# OTHER. If an mp3 file already has an image embedded, but in a different tag,
# say FRONT_COVER (find out with: eyeD3 <audio-file>), do the following:
#  $ eyeD3 -i . <audio-file> # saves all embedded images to the current directory
#  $ embed.sh <audio-file> FRONT_COVER.jpg

function usage()
{
  echo "usage: $0 <audio-file> [image-file]"
  echo "if image-file is not given, default to cover.jpg"
}

if [ $# -eq 1 ]
then
  AUDIO_FILE=$1
  IMAGE_FILE="cover.jpg"
else if [ $# -eq 2 ]
then
  AUDIO_FILE=$1
  IMAGE_FILE=$2
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
