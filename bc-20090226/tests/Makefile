# Copyright (C) Simon Wright <simon@pushface.org>.
#
#      This program is free software; you can redistribute it
#      and/or modify it under the terms of the Ada Community
#      License which comes with this Library.
#
#      This program is distributed in the hope that it will be
#      useful, but WITHOUT ANY WARRANTY; without even the implied
#      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#      PURPOSE. See the Ada Community License for more details.
#      You should have received a copy of the Ada Community
#      License with this library, in the file named "Ada Community
#      License" or "ACL". If not, contact the author of this library
#      for a copy.
#

# $Id: Makefile 1394 2009-01-13 21:10:15Z simonjwright $

# (GNU) Makefile for test of Booch Components distribution.

ifneq (, $(findstring CYGWIN, $(sys)))
  EXE = .exe
else
  EXE =
endif

GNATMAKE=gnatmake

#####################
# Default build rule
%$(EXE): %.adb
	$(GNATMAKE) -Ptests $<

########
# Tests

test: testable
	for t in $(TESTABLE); do \
	  echo running $$t; \
	  ./$$t 2>&1; \
	done

TESTABLE += avl_test$(EXE)
TEST_SRC += avl_test.ad[bs] avl_test_support.ad[bs]
avl_test$(EXE): force

TESTABLE += bag_test$(EXE)
TEST_SRC += bag_test.ad[bs] bag_test_support.ad[bs]
bag_test$(EXE): force

TESTABLE += collection_test$(EXE)
TEST_SRC += collection_test.ad[bs] collection_test_support.ad[bs]
collection_test$(EXE): force

TESTABLE += deque_test
TEST_SRC += deque_test.ad[bs] deque_test_support.ad[bs]
deque_test$(EXE): force

TESTABLE += graph_test$(EXE)
TEST_SRC += graph_test.ad[bs] graph_test_support.ad[bs]
graph_test$(EXE): force

TESTABLE += list_test$(EXE)
TEST_SRC += list_test.ad[bs] list_test_support.ad[bs]
list_test$(EXE): force

TESTABLE += map_test$(EXE)
TEST_SRC += map_test.ad[bs] map_test_support.ad[bs] chunks.ad[bs]
map_test$(EXE): force

TESTABLE += ordered_collection_test$(EXE)
TEST_SRC += ordered_collection_test.ad[bs] \
 ordered_collection_test_support.ad[bs]
ordered_collection_test$(EXE): force

TESTABLE += ordered_queue_test$(EXE)
TEST_SRC += ordered_queue_test.ad[bs] ordered_queue_test_support.ad[bs]
ordered_queue_test$(EXE): force

TESTABLE += queue_test$(EXE)
TEST_SRC += queue_test.ad[bs] queue_test_support.ad[bs]
queue_test$(EXE): force

TESTABLE += ring_test$(EXE)
TEST_SRC += ring_test.ad[bs] ring_test_support.ad[bs] \
tests-rings.ad[bs]
ring_test$(EXE): force

TESTABLE += set_test$(EXE)
TEST_SRC += set_test.ad[bs] set_test_support.ad[bs]
set_test$(EXE): force

TESTABLE += smart_test$(EXE)
TEST_SRC += smart_test.ad[bs] smart_test_support.ad[bs] \
tests-auto_pointers.ad[bs]
smart_test$(EXE): force

TESTABLE += sort_test$(EXE)
TEST_SRC += sort_test.adb
sort_test$(EXE): force

TESTABLE += stack_test$(EXE)
TEST_SRC += stack_test.ad[bs] stack_test_support.ad[bs]
stack_test$(EXE): force

TESTABLE += stream_test$(EXE)
TEST_SRC += stream_test.ad[bs] stream_test_support.ad[bs] \
stream_test_support-tc[bdu].ad[bs]
stream_test$(EXE): force

TESTABLE += tree_test$(EXE)
TEST_SRC += tree_test.ad[bs] tree_test_support.ad[bs] \
tests-avl_trees.ad[bs] tests-multiway_trees.ad[bs]
tree_test$(EXE): force

testable: .build $(TESTABLE)

TEST_SRC += assertions.ad[bs] global_heap.ad[bs] \
tests-support.ad[bs] tests-items.ad[bs] \
tests-main.adb tests.ads

dist:
	mkdir $(DIST)/tests
	cp $(TEST_SRC) Makefile tests.gpr $(DIST)/tests/ 

.build:
	mkdir $@

clean::
	rm -f $(TESTABLE) .build

.PHONY: clean dist force
