#!/usr/bin/env bash

subcommand=$1
shift 1

if [ "$subcommand" == "script" ]; then
    aq2 "$@"
elif  [ "$subcommand" == "stack" ]; then
    aqstack "$@"
elif  [ "$subcommand" == "lrgb" ]; then
    aqlrgb "$@"
else
    echo "Unknown subcommand, choose one of the following:"
    echo "   stack    will run the (legacy) stacking application"
    echo "   lrgb     will run the (legacy) LRGB compositor"
    echo "   script   will run the new Aquila scripting language"
    echo
    echo "Examples:"
    echo
    echo "docker run -it -v \$(pwd):/work --workdir /work aquila stack final *.fits -o stack.fits"
    echo "docker run -it -v \$(pwd):/work --workdir /work aquila lrgb L.fits R.fits G.fits B.fits -best -o LRGB.png"
    echo "docker run -it -v \$(pwd):/work --workdir /work aquila script script.txt"
    echo
    echo "            (in development)"
    exit 1
fi