#!/bin/sh
issue=sept2004
base=~/download/adaletter/
download=$base/www.sigada.org/ada_letters/issues/$issue/
echo '<?xml version="1.0" encoding="koi8-r"?>'
echo "<folder name='$issue'>"
echo "<descr lang='ru'>Выпуск $issue</descr>"
echo "<link id='$issue.$file'"
dir=`echo $download|sed -e 's#.*//##'`
echo " home='http://$dir'>"
for I in `find $download -iname *.pdf | sed -e 's/ /%20/g'`
do
   url=`echo $I|sed -e 's#.*//##'`
   file=`basename $url .pdf`
   echo "<file id='$issue.$file.raw'>"
   echo '<descr lang="ru">'
   echo $url
   echo '</descr>'
   echo '<descr lang="en">'
   echo $url
   echo '</descr>'
   echo "<download type='file'"
   echo " url='http://$url' />"
   echo "</file>"
   echo ""
done
   echo "</link>"
echo "</folder>"

