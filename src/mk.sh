cd ../build
INC="-I../src"
INC="$INC -I/home/max/AWS/include/aws"
INC="$INC -I/home/max/AWS/include/aws/components"
INC="$INC -I/home/max/AWS/include/aws/components/ai302"
INC="$INC -I/home/max/AWS/lib/aws"
LIB="$LIB -L/home/max/AWS/lib/aws"
LIB="$LIB -laws -lai302 -laws_include -laws_nossl -laws_ssl"
gnatmake -O2 $INC ada_ru -largs $LIB
#gnatmake -g $INC ada_ru -largs $LIB -bargs -E

