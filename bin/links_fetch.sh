#!/bin/sh
MY_BIN=`dirname $0`
. $MY_BIN/my_conf

function download ()
{
  WGET="wget --directory-prefix=$2 --execute continue=off --timestamping"
  case "$1" in
    SF:*)
       sf_url=`echo $1 |sed -e s#SF:#$SF#`
    ;;
    *)
       sf_url=$1
    ;;    
  esac;
  echo Download: $sf_url
  $WGET $sf_url
}

function part ()
{
  FILE=`echo $1|sed -e "s/%20/ /g"`
  TARGET="$CD/$FILE"
  SIZE=$3
  DIR=`dirname "$TARGET"`
  [ -d $DIR ] || mkdir -p $DIR
  STORE_DIR=`dirname "$STORE/$FILE"`
  [ -d $STORE_DIR ] || mkdir -p $STORE_DIR
  if [ -f "$STORE/$FILE" ]
  then
    OLD_SIZE=`stat "$STORE/$FILE" |grep "Size:"|cut -f4 -d\ `
    if [ .$SIZE. == .. ]
    then
      echo Size ${STORE}$FILE is null
    elif [ $OLD_SIZE -ne $SIZE ]
    then
      mv "${STORE}$FILE" "${STORE}$1_old"
      echo Size $FILE changed
      download $2 `dirname "$STORE/$FILE"`
    fi
  else
     download $2 `dirname "$STORE/$FILE"`
  fi
  [ -r "$STORE/$FILE" ] && ln -s "`revpath $DIR`$STORE/$FILE" "$TARGET"
   MD5=`jigdo-file md5 --report quiet "$TARGET" |cut -f1 -d\ `
   if [ .$MD5. = .. ]
   then
     echo "Cant download " $*
     exit 1
   fi
  echo ${MD5}=$2 >> $BUILD/jigdo.parts
}

function date_one ()
{
  DATE=`date +"%d %b" -r ${STORE}$2`
  echo $DATE $1 $3
  echo '<tr><td><a href="soft.html#'$1'">'$DATE $1 $3'</a></td></tr>' \
   >> $BUILD/diff.txt
}

rm -fr $BUILD/*
rm -rf $CD/*

xsltproc -o $BUILD/links.tmp $LINKS/copy.xsl $LIST
if [ .$1. != .nofetch. ] 
then
  $FETCH $BUILD/links.tmp $NEW

  if [ $? != 0 ]
  then
     echo Version calucation failed: $?
     exit
  fi
fi

#make jigdo parts
xsltproc -o $BUILD/jigdo.parts.tmp $LINKS/list.xsl $BUILD/links.tmp
#exit
. $BUILD/jigdo.parts.tmp

# find difference
xsltproc --param path \'$LINKS\' -o $BUILD/diff.sh $LINKS/diff.xsl $OLD
if [ -s $BUILD/diff.sh ]
then
   echo Found new versions
   DT=date_one
   . $BUILD/diff.sh
fi

# make full catalog listing
xsltproc -o $BUILD/soft.xml $LINKS/to_docbook4.xsl $LIST

# make list of catalog
xsltproc --param kind "'list'" \
  --param tree_root \'$BUILD\' \
  -o $BUILD/soft.xml $LINKS/to_docbook4.xsl $LIST

# make tree of catalog
xsltproc --param kind "'tree'" \
  --param tree_root \'$BUILD\' \
  -o $BUILD/soft.xml $LINKS/to_docbook4.xsl $LIST

exit

mkisofs -J -R -f -oout/ada-ru.iso $CD

jigdo-file make-template                    \
            --image out/ada-ru.iso          \
            --jigdo tmp/ada-ru.jigdo        \
            --template out/ada-ru.template  \
            --report=noprogress             \
            $CD//

sed -n '0,/\[Servers\]/p' tmp/ada-ru.jigdo > out/ada-ru.jigdo

cat >> out/ada-ru.jigdo << EOF
SF=http://belnet.dl.sourceforge.net/sourceforge/
SF=http://twtelecom.dl.sourceforge.net/sourceforge/
SF=http://cesnet.dl.sourceforge.net/sourceforge/
EOF

echo "" >> out/ada-ru.jigdo
echo "[Parts]" >> out/ada-ru.jigdo
cat tmp/jigdo.parts >> out/ada-ru.jigdo
