#!/bin/bash

echo ==========good============
for file in `ls good/*.lat`; do
    out=good/`basename $file .lat`.output
	echo $file ... `./compile.pl $file | diff -q - $out`
done;

echo ==========bad=============
for file in `ls bad/*.lat`; do
	echo -n $file ... 
	./compile.pl $file
done;
