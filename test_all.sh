#!/bin/bash

if [ "$1" == "" ] || [ $1 == "interpreter" ]; then
    echo ==========good============
    for file in `ls good/*.lat`; do
        out=good/`basename $file .lat`.output
        O=`./latc.pl -m eval $file | diff -q - $out`
        if [ $? != 0 ]; then exit; fi
	    echo $file ... $O
    done;
elif [ "$1" == "frontend" ]; then
    echo ==========bad=============
    for file in `ls bad/*.lat`; do
	    echo -n $file ... 
	    ./latc.pl $file
    done;
else
    for file in `ls good/*.lat`; do
        out=good/`basename $file .lat`.output
        PROG=`./latc $file`
        if [ $? != 0 ]; then exit; fi
        echo compiled $file ... `$PROG | diff -s -q - $out`
    done;
fi;
