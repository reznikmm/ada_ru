#!/bin/sh
function do_one ()
{
sed -f tokens.sed $1 > $1.xml
}

do_one ssqrt.adb
do_one ssqrt.ads
do_one ssqrt_test.adb
do_one asynch_transfer.adb
do_one entry_family.adb
