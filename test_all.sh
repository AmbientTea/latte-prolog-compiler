#!/bin/bash

function test_bad() {
    DIR=$1;
    HEADER=$2;
    
    echo $HEADER:
    for file in `ls $DIR/*.lat`; do
    echo $file ...  `./latc $file 2>&1 >/dev/null | head -n2 | tr "\n" "  "`
    done;
}

function test_good() {
    DIR=$1;
    HEADER=$2;
    
    echo $HEADER:
    
    
    for file in `ls $DIR/*.lat`; do
        out=`dirname $file`/`basename $file .lat`.output
        echo -n "compile $file ... "
        
        PROG=`./latc $file 2> >(head -n 1 | tr -d "\n" 1>&2 )`
        if [ $? != 0 ]; then exit; fi
        
        echo " ..." `$PROG | diff -s -q - $out | grep -o "\(differ\|identical\)"`
    done;
}

echo ========== FRONTEND TESTS: =============
test_bad "tests/bad/" "BASIC"

echo ========== COMPILER TESTS: =============
test_good "tests/good/" "BASIC"
test_good "tests/mrjp-tests/good/basic" "COMMUNITY BASIC"


