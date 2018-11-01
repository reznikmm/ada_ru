#!/bin/bash
ROOT="/tmp/ada_ru"
SBOX="$ROOT/sandbox"
STORE="$ROOT/store"
JOBS="$ROOT/jobs"
GPR="$ROOT/default.gpr"
WAIT="/usr/bin/inotifywait --monitor --event close_write --format %f $JOBS"

[ -d $JOBS ] || mkdir -p $JOBS
[ -d $SBOX ] || mkdir -p $SBOX
[ -f $GPR ] || cat > $GPR << EOF
project Default is
   for Source_Dirs use ("$SBOX");

   package Compiler is
      for Default_Switches ("Ada") use (
        "-gnat12",        --  Enable Ada 2012 mode
        "-gnatW8",        --  Interpret source files as UTF-8 encoded.
        "-gnatwe",        --  Warnings as errors
        --  Enable warnings:
        "-gnatwa",
        "-gnatyaAbcdefhiIkmnoOprsStux");
   end Compiler;
end Default;
EOF

do_compile()
{
    pushd $SBOX/
    rm -rf *
    key=$1
    ln -s $STORE/$key/_source.adb .
    gnatchop -r _source.adb
    rm -f _source.adb
    gprbuild -P $GPR 2> $STORE/$key/gcc-error.txt_
    mv $STORE/$key/gcc-error.txt_ $STORE/$key/gcc-error.txt
    popd
}


$WAIT | while read key ; do
    echo $key
    text=`cat $JOBS/$key`
    rm $JOBS/$key
    case $text in
        'compile')
            do_compile $key
            ;;
    esac
    rm -rf `ls $STORE --sort=time |tail -n +100`
done
