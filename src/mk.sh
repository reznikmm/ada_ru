cd ../build
INC="-I../src"
LIBADA=/opt/gnatlib-5.03/debug
INC="$INC -I$LIBADA/include/aws"
INC="$INC -I$LIBADA/include/aws/components"
INC="$INC -I$LIBADA/include/aws/components/ai302"
INC="$INC -I$LIBADA/lib/aws"
LIB="$LIB -L$LIBADA/lib/aws"
LIB="$LIB -laws -lai302 -laws_include -laws_nossl -laws_ssl"
#gnatmake -O2 $INC ada_ru -largs $LIB
gnatmake -g $INC ada_ru -largs $LIB -bargs -E
