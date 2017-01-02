#!/bin/bash

echo ========== testing frontend: =============
for file in `ls bad/*.lat`; do
    echo -n $file ... 
    ./latc.pl $file
done;

if [ "$1" == "interpreter" ] || [ "$1" == "all" ]; then
    echo ========== testint interpreter: ============
    for file in `ls good/*.lat`; do
        out=good/`basename $file .lat`.output
        O=`./latc.pl -m eval $file | diff -s -q - $out`
        if [ $? != 0 ]; then exit; fi
	    echo $file ... $O
    done;
fi;

if [ "$1" == "" ] || [ "$1" == "compiler" ] || [ "$1" == "all" ]; then
    echo ========== testing compiler: =============
    for file in `ls good/*.lat`; do
        out=good/`basename $file .lat`.output
        PROG=`./latc $file`
        if [ $? != 0 ]; then exit; fi
        echo compiled $file ... `$PROG | diff -s -q - $out`
    done;
fi;
