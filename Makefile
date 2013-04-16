###
# You might want to adjust these paths.

WEBSHARPER_BIN = /home/kirrun/misc/WebSharper/WebSharper.v4.5.exe

ASSEMBLIES_DIR_DOTNET := /usr/lib64/mono/4.0
ASSEMBLIES_DIR_FSHARP := $(ASSEMBLIES_DIR_DOTNET)
ASSEMBLIES_DIR_WEBSHARPER := /home/kirrun/misc/WebSharper

#
##

###
# You probably don't want to touch this.

ASSEMBLIES_DOTNET := mscorlib System System.Core System.Web System.Numerics
ASSEMBLIES_FSHARP := FSharp.Core
ASSEMBLIES_WEBSHARPER := IntelliFactory.Formlet IntelliFactory.Html IntelliFactory.JavaScript IntelliFactory.Reactive IntelliFactory.WebSharper.Collections IntelliFactory.WebSharper.Control IntelliFactory.WebSharper.Core IntelliFactory.WebSharper IntelliFactory.WebSharper.Dom IntelliFactory.WebSharper.Ecma IntelliFactory.WebSharper.Formlet IntelliFactory.WebSharper.Html IntelliFactory.WebSharper.Html5 IntelliFactory.WebSharper.JQuery IntelliFactory.WebSharper.Sitelets IntelliFactory.WebSharper.Testing IntelliFactory.WebSharper.Web IntelliFactory.Xml

# Order matters!
INPUT := src/fsutil.fs src/jsutil.fs src/filearray.fs src/bytecode.fs src/dex.fs src/shared.fs src/interpret.fs src/manager.fs src/vm.fs

ALL_ASSEMBLIES := $(foreach kind,DOTNET FSHARP WEBSHARPER,$(ASSEMBLIES_$(kind):%=$(ASSEMBLIES_DIR_$(kind))/%.dll))


build : build/output.js build/run.html build/gLong.js build/WebSharper/

build/all.dll : $(INPUT)
	fsharpc --target:library --noframework $(ALL_ASSEMBLIES:%=-r:%) -o:$@ $^

build/output.js : build/all.dll
	mono $(WEBSHARPER_BIN) $(ALL_ASSEMBLIES:%=-r %) -js $@ $< /tmp/useless.dll
	rm /tmp/useless.dll

build/run.html : src/run.html
	rsync -p $< $@

build/gLong.js : vendor/gLong.js
	rsync -p $< $@

build/WebSharper/ : vendor/WebSharper/
	rsync -pr $< $@

.PHONY : clean
clean :
	rm -rf build/*
