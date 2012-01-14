#!/bin/sh

basedir=`dirname "$0"`
if [ "$basedir" = "." ] ; then
    basedir="$PWD"
fi

installjar="$basedir/Royal/build/archives/Royal-install-trunk.jar"

if [ -e "$installjar" ] ; then
    java -jar "$installjar"
else
    echo "Please build the project first with build.sh"
fi

