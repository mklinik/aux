#!/bin/bash

if [ $# -ne 2 ]
then
    echo "usage: swap.sh <name1> <name2>
    swap two files or directories"
    exit 1
fi

NAME1=$1
NAME2=$2

if [ -e $NAME1 -a -e $NAME2 ]
then
    TMPDIR=`mktemp -d --tmpdir=.`
    BASENAME1=`basename $NAME1`
    mv -i "$NAME1" "$TMPDIR"
    mv -i "$NAME2" "$NAME1"
    mv -i "$TMPDIR/$BASENAME1" $NAME2
    rmdir $TMPDIR
else
    echo "error: one or both of the specified files do not exist"
    exit 1
fi

exit 0
