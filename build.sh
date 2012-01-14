#!/bin/sh

basedir=`dirname "$0"`
if [ "$basedir" = "." ] ; then
    basedir="$PWD"
fi

buildall()
{
    buildhelp
    build    
}

buildhelp()
{
    cd "$basedir/RoyalHelp"
    ant build
}

build()
{
    cd "$basedir/Royal"
    ant build
    ant installer
}


if [ $# -eq 0 ] ; then
    buildall
elif [ $# -eq 1 ] ; then
    case "$1" in
        "RoyalHelp") buildhelp;;
        "Royal") build;;
        "all") buildall;;
        *) echo "use $0 <[folder]|all>"
    esac
fi


