all:
	gprbuild -p -P gnat/server.gpr -XAWS_BUILD=relocatable

deploy: all
	mkdir -p install/WEB-INF/lib/x86_64-linux
	cp gnat/.libs/libada_ru.so install/WEB-INF/lib/x86_64-linux
	cp web.xml install/WEB-INF/
