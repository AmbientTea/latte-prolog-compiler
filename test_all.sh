#!/bin/bash

function test_bad() {
    DIR=$1;
    HEADER=$2;
    
    echo $HEADER:
    for file in `ls $DIR/*.lat`; do
    printf "%-60s ... " $file
    echo `./latc $file 2>&1 >/dev/null | head -n2 | tr "\n" "  "`
    done;
}

function test_good() {
    DIR=$1;
    HEADER=$2;
    
    echo $HEADER:
    
    
    for file in `ls $DIR/*.lat`; do
        out=`dirname $file`/`basename $file .lat`.output
        printf "compile %-60s ... " $file
        
        PROG=`./latc $file 2> >(head -n 1 | tr -d "\n" 1>&2 )`
        if [ $? != 0 ]; then exit; fi
        
        IN=`dirname $file`/`basename $file .lat`.input
        if [ -e "$IN" ]; then
            IND="$IN"
        else
            IND=/dev/zero
        fi
        
        echo " ..." `$PROG < $IND | diff -s -q - $out | grep -o "\(differ\|identical\)"`
    done;
}

echo ========== FRONTEND TESTS: =============
test_bad "tests/bad/" "BASIC"
test_bad "tests/mrjp-tests/bad/semantic" "COMMUNITY BASIC"
test_bad "tests/mrjp-tests/bad/runtime" ""

echo ========== COMPILER TESTS: =============
test_good "tests/good/" "BASIC"
test_good "tests/mrjp-tests/good/basic" "COMMUNITY BASIC"
test_good "tests/extensions/struct" "STRUCT"
