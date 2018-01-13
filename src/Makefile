VERSION = .
INSTALL = install -c
INSTALL_data = $(INSTALL) -m 0444

GPRBUILDDB ?= /home/max/net/adalib.gpl.2017/share/gprconfig/
MATRESHKA ?= /home/max/net/matreshka/

JSRTL = testsuite/a2js/library/league.js \
	testsuite/a2js/library/league-strings.js \
	source/web/tools/a2js/rtl/standard.js

JSFILES = logins.js

all:
	gprbuild -p -P gnat/server.gpr -XAWS_BUILD=relocatable

deploy: all js
	mkdir -p install/WEB-INF/lib/x86_64-linux
	cp gnat/.libs/libada_ru.so install/WEB-INF/lib/x86_64-linux
	cp web.xml install/WEB-INF/
	$(INSTALL) -d install/$(VERSION)/js
	$(INSTALL_data) $(addprefix gnat/.js/, $(JSFILES)) \
			install/$(VERSION)/js/

	$(INSTALL_data) $(addprefix $(MATRESHKA)/,$(JSRTL)) \
			install/$(VERSION)/js/

js:
	gprbuild -p -P gnat/client.gpr --db $(GPRBUILDDB) --target=javascript
