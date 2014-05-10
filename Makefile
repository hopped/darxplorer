# Makefile for DARXplorer
# make compiler - guess what
# make function
# make tf256 - compiles test
#
# set up flags
OBJDIR=obj
CORES_TIMES_TWO=4
GNATFLAGS=-O3 -m -p -ggdb -gnatVa -gnato -fstack-check -gnata -gnatyx -gnat05 -j$(CORES_TIMES_TWO)

# flags for gnatbind
BARGS=-bargs -E

DIRECTORY=..
MAIN_SOURCES=$(DIRECTORY)/src
COMPILER_SOURCES=$(MAIN_SOURCES)/compiler
PARSER_SOURCES=$(COMPILER_SOURCES)/parser
SUPPORT_SOURCES=$(PARSER_SOURCES)/support
LEXER_SOURCES=$(COMPILER_SOURCES)/lexer
FUNCTION_SOURCES=$(MAIN_SOURCES)/function
ADA_BOOCH=$(DIRECTORY)/bc-20090226
BC_SOURCES=$(ADA_BOOCH)/src
BC_DEMOS=$(ADA_BOOCH)/demos

### fix because max uses older gnu-gnat, dennis uses
## new one in search path
GNATMAKE=$(shell which gnatmake)
## gnatmake not found in path, use old default
ifeq ("","$(GNATMAKE)")
	GNATMAKE='/usr/local/ada-4.3/bin/gnatmake'
endif

## gnatmake tracks dependencies, so all
## targets need to be phony: rebuild them, if commanded
.PHONY: have-dirs clean-binder test-lexer compiler tf256 trudy


all:
	echo $(GNATMAKE)

have-dirs:
	mkdir -p $(OBJDIR)

## delete those stupid binder files gnatbind needs
# to create in working directory
clean-binder:
	find . -name 'b~*' -delete

test-lexer: have-dirs
	cd $(OBJDIR); \
	$(GNATMAKE) $(GNATFLAGS) \
		-I$(MAIN_SOURCES) \
		-I$(COMPILER_SOURCES) \
		-I$(LEXER_SOURCES) \
		$(LEXER_SOURCES)/test_lexer.adb \
		$(BARGS); \
	mv test_lexer $(DIRECTORY)/lexer
	make clean-binder

compiler: have-dirs
	cd $(OBJDIR); \
	$(GNATMAKE) $(GNATFLAGS) \
		-I$(MAIN_SOURCES) \
		-I$(BC_SOURCES) \
		-I$(BC_DEMOS) \
		-I$(COMPILER_SOURCES) \
		-I$(PARSER_SOURCES) \
		-I$(SUPPORT_SOURCES) \
		-I$(LEXER_SOURCES) \
		$(COMPILER_SOURCES)/dxpl-compiler-cli.adb \
		$(BARGS)
	make clean-binder

function: have-dirs
	cd $(OBJDIR); \
	$(GNATMAKE) $(GNATFLAGS) \
		-I$(MAIN_SOURCES) \
		-I$(BC_SOURCES) \
		-I$(BC_DEMOS) \
		-I$(FUNCTION_SOURCES) \
		dxpl-darxplorer.adb \
		$(BARGS); \
	mv dxpl-darxplorer $(DIRECTORY)/darxplorer
	make clean-binder

run-compiler: compiler
	cd $(OBJDIR); \
	mv dxpl-compiler-cli $(DIRECTORY)/dxpl-compiler-cli
	./dxpl-compiler-cli $(ARG)

run: run-compiler function
	./darxplorer

clean-function:
	rm -rfv src/function/*.adb
	rm -rfv src/function/*.ads

clean:
	rm -rfv $(OBJDIR)
	rm -f test_lexer
	make clean-binder
