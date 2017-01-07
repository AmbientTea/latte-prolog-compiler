#!/bin/bash

echo ========== testing frontend: =============
for file in `ls tests/bad/*.lat`; do
    echo $file ...  `./latc $file 2>&1 >/dev/null | head -n2 | tr "\n" "  "`
done;

if [ "$1" == "" ] || [ "$1" == "compiler" ] || [ "$1" == "all" ]; then
    echo ========== testing compiler: =============
    for file in `ls tests/good/*.lat`; do
        out=`dirname $file`/`basename $file .lat`.output
        echo -n "compile $file ... "
        
        PROG=`./latc $file 2> >(head -n 1 | tr -d "\n" 1>&2 )`
        if [ $? != 0 ]; then exit; fi
        
        echo " ..." `$PROG | diff -s -q - $out`
    done;
fi;
