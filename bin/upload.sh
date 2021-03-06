#!/bin/sh
MY_BIN=`dirname $0`
. $MY_BIN/my_conf

[ -f $OUT/soft.html ] && gzip -9 $OUT/soft.html

cd $OUT

for I in `find .// -type f`
do
  loc=`echo $I|sed -e "s#.*//##"`
  diff --brief $loc ../ref/$loc
  if [ $? != 0 ]
  then
    echo post http://www.ada-ru.org $1 $2 $loc
    if ../bin/post http://www.ada-ru.org $1 $2 $loc
    then
       mv -f $loc ../ref/$loc
    else
       echo $loc failed
       exit
    fi
  fi
done
