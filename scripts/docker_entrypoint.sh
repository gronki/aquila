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
    echo "   docker run -it -v \"\$PWD\":/work aquila stack final *.fits -o stack.fits"
    echo "   docker run -it -v \"\$PWD\":/work aquila lrgb L.fits R.fits G.fits B.fits -best -o LRGB.png"
    echo "   docker run -it -v \"\$PWD\":/work aquila script script.txt"
    echo
    echo "It is also possible to add the following in your .bashrc to shorten the command:"
    echo
    echo "   function aquila {"
    echo "      docker run -it -v \"\$PWD\":/work aquila \"\$@\""
    echo "   }"
    echo
    echo "Then the invocation becomes:"
    echo
    echo "   aquila stack final *.fits -o stack.fits"
    echo
    echo "Everything is still in development, report bugs at:"
    echo "   https://github.com/gronki/aquila/issues"
    exit 1
fi