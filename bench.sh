#!/usr/bin/env bash

if [ ! \( -f 'benchdata/in.txt' \) ]; then
    man gcc > benchdata/in.txt
    if [ ! \( $? = 0 \) ]; then
	echo "bench.sh uses \`man gcc\` as input data." 1>&2
	echo "Make it available or prepare another input data by yourself" 1>&2
	echo "as text file 'benchdata/in.txt'. It's better to be an english" 1>&2
	echo "file of size about 1MB." 1>&2
	exit 1
    fi
fi

rm -f benchdata/result.csv
stack bench --ba='--csv=benchdata/result.csv'
