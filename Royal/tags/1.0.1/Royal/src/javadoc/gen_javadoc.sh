#!/bin/sh

bin="/usr/bin/javadoc"
gendir="build"

if [ ! -e "$bin" ] ; then
	echo -ne "Javadoc doesn't seem to be installed.\nJavadoc executable : "
	read bin;
fi

if [ ! -e "$gendir" ] ; then
	mkdir  "$gendir"
fi

dname=`dirname "$0"`
cd "$dname/$gendir"

find ../../java/net/sf/royal -type f -name "*.java" | xargs "$bin" -private
