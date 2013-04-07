WEBSHARPER_BIN = /home/kirrun/misc/WebSharper/WebSharper.v4.5.exe

FSHARP_OPTS := $(shell cat builder/fsarg.txt)
WEBSHARPER_OPTS := $(shell cat builder/wsarg.txt)


# Order matters!
INPUT := src/jsutil.fs src/filearray.fs srd/bytecode.fs src/dex.fs src/shared.fs src/interpret.fs src/vm.fs


build : build/output.js build/run.html

build/all.dll : $(INPUT)
	fsharpc $(FSHARP_OPTS) -o:$@ $^

build/output.js : build/all.dll
	mono $(WEBSHARPER_BIN) $(WEBSHARPER_OPTS) -js $@ $< /tmp/useless.dll
	rm /tmp/useless.dll

build/run.html : src/run.html
	cp $< $@

.PHONY : clean
clean :
	rm -rf build/*
