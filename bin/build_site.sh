#!/bin/sh
MY_BIN=`dirname $0`
. $MY_BIN/my_conf

[ -f $BUILD/last_mod.ent ] && rm $BUILD/last_mod.ent

for i in $CONT/*.xml
do
  name=`basename $i .xml`
  date=`date -r $i +"%d %b %Y" `
  echo "<!ENTITY last_mod_$name \"$date\">" | \
    iconv -t utf-8 >> $BUILD/last_mod.ent
done

xsltproc -o $BUILD/dict_ru.tbl $CONT/dict_ru.xsl $CONT/dict.xml
xsltproc -o $BUILD/dict_old.tbl $CONT/dict_ru.xsl $CONT/dict_old.xml
xsltproc -o $BUILD/news_ru.tbl $CONT/news.xsl $CONT/news.xml
xsltproc -o $BUILD/old_news.xml $CONT/old_news.xsl $CONT/news.xml
iconv -t utf-8 $CONT/soft_page.xml > $BUILD/soft_page.xml

xsltproc -o $BUILD/autolayout.xml $WEBSITE/autolayout.xsl $CONT/layout.xml
#xsltproc --param output-root \'../aaa\' \
#  $WEBSITE/chunk-tabular.xsl $BUILD/autolayout.xml
xsltproc $CONT/website.xsl $BUILD/autolayout.xml

ln -s ../build/autolayout.xml $CONT
xsltproc -o $OUT/index.html $CONT/website_home.xsl $CONT/homepage.xml

cp -r $MISC/graphics $OUT
cp -r $CONT/test.css $OUT

xsltproc -o $BUILD/autolayout.xml $WEBSITE/autolayout.xsl $CONT/layout_cd.xml

xsltproc --param output-root \'$CD\' \
  --param nav.icon.path \'www.ada-ru.org/graphics/navicons/\' \
  --param toc.spacer.image \'www.ada-ru.org/graphics/blank.gif\' \
  $CONT/website.xsl $BUILD/autolayout.xml

xsltproc --param nav.icon.path \'www.ada-ru.org/graphics/navicons/\' \
  --param toc.spacer.image \'www.ada-ru.org/graphics/blank.gif\' \
  -o $SITE/index.html $CONT/website_home.xsl $CONT/homepage.xml

cp -r $MISC/* $SITE
cp -r $CONT/test.css $SITE
cp $MY_ROOT/autoexec/* $CD
rm -f $CD/.cvsignore
