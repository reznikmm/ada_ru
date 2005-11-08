cd ../build
INC="-I../src"
INC="$INC -I/home/max/AWS/include"
INC="$INC -I/home/max/AWS/components"
INC="$INC -I/home/max/AWS/components/ai302"
INC="$INC -I/home/max/AWS/lib"
LIB="$LIB -L/home/max/AWS/lib/"
LIB="$LIB -laws -laws_nossl"
gnatmake -O2 $INC ada_ru -largs $LIB
#gnatmake -g $INC ada_ru -largs $LIB -bargs -E

