#!/bin/sh
MY_BIN=`dirname $0`
. $MY_BIN/my_conf

rm -rf $BUILD/[^Ck]*
rm -rf $CD/[^Ck]*
rm -rf $OUT/[^Ck]*
