#!/bin/bash

function fatal() {
    echo "error: $@" 1>&2;
    exit 1;
}

if [ $# -lt 2 ] || [ $# -gt 3 ]; then
    echo "Usage: $0 [file] [blocks|instrs] <limit>";
    exit 1;
fi

file=$1;
name=$2;
max=$3;

if [ "$name" == "blocks" ]; then
    col=4;
    [ -n "$max" ] || max=100;
elif [ "$name" == "instrs" ]; then
    col=5;
    [ -n "$max" ] || max=1000;
else
    fatal "invalid argument \"$name\"" 1>&2;
fi

[ -f "${file}" ] || fatal "Invalid file \"$file\"";

for type in complete incomplete; do
    if [ "${type}" == "complete" ]; then
        value="true";
    else
        value="false";
    fi

    cat ${file} | grep ",${value}," | cut -f${col} -d',' | sort -n | uniq -c | \
        awk -v "name=${name}" -v "lim=${max}" 'BEGIN {print name ",count"} $2 <= lim {print $2 "," $1}' > ${type}.csv
done
