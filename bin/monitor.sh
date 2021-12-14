#!/bin/bash
SRC="$HOME/source/ada_ru/game"
ROOT="/tmp/ada_ru"
SBOX="$ROOT/sandbox"
STORE="$ROOT/store"
JOBS="$ROOT/jobs"
GPR="$ROOT/default.gpr"
MISSION_RUN_GPR="$ROOT/mission_run.gpr"
MISSION_CHECK_GPR="$ROOT/mission_check.gpr"
WAIT="/usr/bin/inotifywait --monitor --event close_write --format %f $JOBS"

[ -d $JOBS ] || mkdir -p $JOBS
[ -d $SBOX ] || mkdir -p $SBOX
cat > $GPR << EOF
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

cat > $MISSION_RUN_GPR << EOF
project Mission_Run is
   for Source_Dirs use ("$SBOX", "$SRC");
   for Object_Dir use "$SBOX";
   for Main use ("mission_main.adb");

   package Binder is
      for Switches ("Ada") use ("-W8");
   end Binder;

   package Naming is
      for Specification  ("Mission") use "_source.adb" at 1;
      for Implementation ("Mission") use "_source.adb" at 2;
   end Naming;

   package Compiler is
      for Default_Switches ("Ada") use (
        "-gnata",         --  Enable assertions
        "-gnat2020",      --  Enable Ada 2020 mode
        "-gnatX",         --  Enable Language extensions permitted
        "-gnatW8");       --  Interpret source files as UTF-8 encoded.
   end Compiler;
end Mission_Run;
EOF

cat > $MISSION_CHECK_GPR << EOF
with "ahven";

project Mission_Check is
   MISSION := external ("MISSION", "");
   for Source_Dirs use ("$SBOX", "$SRC", "$SRC/" & MISSION);
   for Object_Dir use "$SBOX";
   for Main use ("mission_check.adb");

   package Binder is
      for Switches ("Ada") use ("-W8");
   end Binder;

   package Naming is
      for Specification  ("Mission") use "_source.adb" at 1;
      for Implementation ("Mission") use "_source.adb" at 2;
   end Naming;

   package Compiler is
      for Default_Switches ("Ada") use (
        "-gnata",         --  Enable assertions
        "-gnat2020",      --  Enable Ada 2020 mode
        "-gnatX",         --  Enable Language extensions permitted
        "-gnatW8");       --  Interpret source files as UTF-8 encoded.
   end Compiler;
end Mission_Check;
EOF


do_gnatmake()
{
    pushd $SBOX/
    key=$1
    ln -s $STORE/$key/_source.adb .
    cat > gnat.adc <<-EOF
    pragma Warnings (Off, "file name does not match unit name");
EOF
    BUILD="gnatmake -o mission_main _source.adb"
    if $BUILD 2> $STORE/$key/gcc-error.txt_ && [ -f ./mission_main ] ; then
        timeout -v 5 ./mission_main &> $STORE/$key/run.txt
    fi
    mv $STORE/$key/gcc-error.txt_ $STORE/$key/gcc-error.txt
    popd
}

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
    BUILD="gprbuild -P $MISSION_RUN_GPR"
    if $BUILD 2> $STORE/$key/gcc-error.txt_ && [ -f $SBOX/mission_main ] ; then
        timeout -v 5 $SBOX/mission_main &> $STORE/$key/run.txt
    fi
    mv $STORE/$key/gcc-error.txt_ $STORE/$key/gcc-error.txt
}

do_mission_check() {
    key=$1
    mission=$2
    user=$3
    ln -s $STORE/$key/_source.adb $SBOX/
    update_text "$mission" "$user"
    BUILD="gprbuild -XMISSION=$mission -P $MISSION_CHECK_GPR"
    if $BUILD 2> $STORE/$key/gcc-error.txt_ && [ -f $SBOX/mission_check ] ; then
        if timeout -v 5 $SBOX/mission_check &> $STORE/$key/run.txt ; then
            mark_solved "$mission" "$user" > $STORE/$key/solved.txt
        fi
    fi
    mv $STORE/$key/gcc-error.txt_ $STORE/$key/gcc-check.txt
}

mark_solved() {
    mission=$1
    user=$2
    echo $user solved $mission
    if [ -n "$user" ]; then
      CMD="insert into solved_missions(nickname,mission) values(:'nick',:'mission');commit;"
      echo $CMD| psql -v mission="$mission" -v nick="$user" mail
    fi
}

update_text(){
    mission=$1
    user=$2
    if [ -n "$user" ]; then
        # Escape backslash and column separator characters
        sed -e 's/[\\~]/\\&/g' $SBOX/_source.adb > $SBOX/import.txt
        psql -v mission="$mission" -v nick="$user" mail <<-EOF
create table temp_text (line serial, text varchar);
\\copy temp_text (text) from '$SBOX/import.txt' with DELIMITER '~' NULL '' ENCODING 'utf-8';
delete from solution_texts where nickname=:'nick' and mission=:'mission';
insert into solution_texts select :'nick', :'mission', line, coalesce(text,'')
from temp_text;
drop table temp_text;
commit;
EOF
    fi
}

$WAIT | while read key ; do
    echo $key
    text=`cat $JOBS/$key`
    rm $JOBS/$key
    rm -rf $SBOX/*
    case $text in
        compile )
            do_compile $key
            ;;
        mission_run )
            do_mission_run $key
            ;;
        mission_check* )
            do_mission_check $key ${text#mission_check}
            ;;
        gnatmake* )
            do_gnatmake $key
            ;;
    esac
    rm -rf `ls $STORE --sort=time |tail -n +100`
done
