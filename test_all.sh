#!/bin/bash

echo ==========good============
for file in `ls good/*.lat`; do
	echo -n $file ...
	./compile.pl $file
	# $file.out > a 
	# diff a good/`basename $file .lat`.output
done;

echo ==========bad=============
for file in `ls bad/*.lat`; do
	echo -n $file ... 
	./compile.pl $file
done;
