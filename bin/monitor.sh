#!/bin/bash
ROOT="/tmp/ada_ru"
SBOX="$ROOT/sandbox"
STORE="$ROOT/store"
JOBS="$ROOT/jobs"
GPR="$ROOT/default.gpr"
MISSION_RUN_GPR="$ROOT/mission_run.gpr"
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
[ -f $MISSION_RUN_GPR ] || cat > $MISSION_RUN_GPR << EOF
project Mission_Run is
   for Source_Dirs use ("$SBOX", "$ROOT");
   for Object_Dir use "$SBOX";
   for Main use ("mission_main.adb");

   package Binder is
      for Switches ("Ada") use ("-shared", "-E", "-W8");
   end Binder;

   package Naming is
      for Specification  ("Mission") use "_source.adb" at 1;
      for Implementation ("Mission") use "_source.adb" at 2;
   end Naming;

   package Compiler is
      for Default_Switches ("Ada") use (
        "-gnata",         --  Enable assertions
        "-gnat12",        --  Enable Ada 2012 mode
        "-gnatW8");       --  Interpret source files as UTF-8 encoded.
   end Compiler;
end Mission_Run;
EOF

cat > $ROOT/mission_main.adb << EOF
with Mission;
pragma Unreferenced (Mission);
procedure Mission_Main is
begin
   null;
end Mission_Main;
EOF


do_compile()
{
    pushd $SBOX/
    key=$1
    ln -s $STORE/$key/_source.adb .
    gnatchop -r _source.adb
    rm -f _source.adb
    gprbuild -P $GPR 2> $STORE/$key/gcc-error.txt_
    mv $STORE/$key/gcc-error.txt_ $STORE/$key/gcc-error.txt
    popd
}

do_mission_run() {
    key=$1
    ln -s $STORE/$key/_source.adb $SBOX/
    if gprbuild -P $MISSION_RUN_GPR 2> $STORE/$key/gcc-error.txt_ && [ -f $SBOX/mission_main ] ; then
        timeout -v 5 $SBOX/mission_main &> $STORE/$key/run.txt
    fi
    mv $STORE/$key/gcc-error.txt_ $STORE/$key/gcc-error.txt
}


$WAIT | while read key ; do
    echo $key
    text=`cat $JOBS/$key`
    rm $JOBS/$key
    rm -rf $SBOX/*
    case $text in
        'compile')
            do_compile $key
            ;;
        'mission_run')
            do_mission_run $key
            ;;
    esac
    rm -rf `ls $STORE --sort=time |tail -n +100`
done
