#!/bin/sh
MY_BIN=`dirname $0`
. $MY_BIN/my_conf

mkisofs -J -R -f -o$BUILD/ada-ru.iso $CD

[ -d $FILES ] || mkdir -p $FILES

[ -f $FILES/ada-ru.jigdo ] && rm -f $FILES/ada-ru.jigdo
[ -f $FILES/ada-ru.template ] && rm -f $FILES/ada-ru.template
[ -f $BUILD/ada-ru.jigdo ] && rm -f $BUILD/ada-ru.jigdo

cat > $BUILD/ada-ru.jigdo << EOF
[Servers]
SF=$SF
SF=http://twtelecom.dl.sourceforge.net/sourceforge/
SF=http://cesnet.dl.sourceforge.net/sourceforge/
ADA_RU=http://www.ada-ru.org/

[Parts]
EOF
cat $BUILD/jigdo.parts >> $BUILD/ada-ru.jigdo

find $CD// -follow -type f -not -name index.html \
           -not -name ada_ru.ico \
           -not -name autorun.exe \
           -not -path "*www.ada-ru.org*" -size +1k |
 \
jigdo-file make-template                    \
            --image $BUILD/ada-ru.iso       \
            --jigdo $FILES/ada-ru.jigdo     \
            --template $FILES/ada-ru.template \
            --report=noprogress             \
            --merge=$BUILD/ada-ru.jigdo     \
            --label ADA_RU=$MISC            \
            --no-servers-section            \
            --files-from=- $MISC//

