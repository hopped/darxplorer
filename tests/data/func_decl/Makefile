# Makefile for testgen
#
# set up flags
OBJDIR=obj
CORES_TIMES_TWO=4
GNATFLAGS=-O3 -m -p -gnatVa -gnato -fstack-check -gnata -gnatyx -gnat05 -j$(CORES_TIMES_TWO)

DIRECTORY=../../../..
MAIN=../../..
MAIN_SOURCES=$(DIRECTORY)/src
COMPILER_SOURCES=$(MAIN_SOURCES)/compiler
PARSER_SOURCES=$(COMPILER_SOURCES)/parser
SUPPORT_SOURCES=$(PARSER_SOURCES)/support
LEXER_SOURCES=$(COMPILER_SOURCES)/lexer
FUNCTION_SOURCES=$(MAIN_SOURCES)/function
ADA_BOOCH=$(DIRECTORY)/bc-20090226
BC_SOURCES=$(ADA_BOOCH)/src
BC_DEMOS=$(ADA_BOOCH)/demos
TG_DIR=../..

### fix because max uses older gnu-gnat, dennis uses
## new one in search path
GNATMAKE=$(shell which gnatmake)
## gnatmake not found in path, use old default
ifeq ("","$(GNATMAKE)")
	GNATMAKE='/usr/local/ada-4.3/bin/gnatmake'
endif

## gnatmake tracks dependencies, so all
## targets need to be phony: rebuild them, if commanded
.PHONY: have-dirs clean-binder

all:
	echo $(GNATMAKE)

have-dirs:
	mkdir -p $(OBJDIR)

## delete those stupid binder files gnatbind needs
# to create in working directory
clean-binder:
	find . -name 'b~*' -delete

testgen: have-dirs
	$(TG_DIR)/tg -p titles -f full testsuite.ts

compiler: testgen
	cd $(OBJDIR); \
	$(GNATMAKE) $(GNATFLAGS) \
		-I$(MAIN_SOURCES) \
		-I$(BC_SOURCES) \
		-I$(BC_DEMOS) \
		-I$(COMPILER_SOURCES) \
		-I$(PARSER_SOURCES) \
		-I$(SUPPORT_SOURCES) \
		-I$(LEXER_SOURCES) \
		../testsuite.adb; \
	mv testsuite $(DIRECTORY)/tg-func_decl
	make clean-binder

run: compiler
	cd $(MAIN); \
	./tg-func_decl

clean:
	rm -rfv $(OBJDIR)
	rm -f testsuite.adb
	make clean-binder
