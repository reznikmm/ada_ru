#!/bin/bash
ROOT="/tmp/ada_ru"
SBOX="$ROOT/sandbox"
STORE="$ROOT/store"
JOBS="$ROOT/jobs"
WAIT="/usr/bin/inotifywait --monitor --event close_write --format %f $JOBS"

[ -d $JOBS ] || mkdir -p $JOBS
[ -d $SBOX ] || mkdir -p $SBOX

do_compile()
{
    pushd $SBOX/
    rm -rf *
    key=$1
    ln -s $STORE/$key/_source.adb .
    gnatchop -r _source.adb
    rm -f _source.adb
    gcc -c *.ad[sb] 2> $STORE/$key/gcc-error.txt
    popd
}


$WAIT | while read key ; do
    echo $key
    text=`cat $JOBS/$key`
    case $text in
        'compile')
            do_compile $key
            ;;
    esac
done
