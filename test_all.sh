#!/bin/bash

if [ "$1" == "" ] || [ $1 == "interpreter" ]; then
    echo ==========good============
    for file in `ls good/*.lat`; do
        out=good/`basename $file .lat`.output
	    echo $file ... `./latc.pl $file | diff -q - $out`
    done;

    echo ==========bad=============
    for file in `ls bad/*.lat`; do
	    echo -n $file ... 
	    ./latc.pl $file
    done;
else
    for file in `ls good/*.lat`; do
        out=good/`basename $file .lat`.output
        PROG=`./latc $file`
        $PROG > $out
        if [ $? != 0 ]; then exit; fi
        echo $file ... `./latc.pl $file | diff -q - $out`
    done;

    echo ==========bad=============
    for file in `ls bad/*.lat`; do
	    echo -n $file ... 
	    ./latc.pl $file
    done;
    
fi;
