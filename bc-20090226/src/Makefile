# Copyright (C) Simon Wright <simon@pushface.org>.

# This package is free software; you can redistribute it and/or
# modify it under terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or
# (at your option) any later version. This package is distributed in
# the hope that it will be useful, but WITHOUT ANY WARRANTY; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU General Public License for more
# details. You should have received a copy of the GNU General Public
# License distributed with this package; see file COPYING.  If not,
# write to the Free Software Foundation, 59 Temple Place - Suite
# 330, Boston, MA 02111-1307, USA.

# $Id: Makefile 1401 2009-02-21 22:10:59Z simonjwright $

# (GNU) Makefile for Booch Components.

all::


#####################
# Default build rule
%: %.adb
	gnatmake $(GNATMAKE_FLAGS) $@

#GCOV_FLAGS = -O0 -ftest-coverage -fprofile-arcs

GNATMAKE_FLAGS = -O2 -g -gnatqQafoy -gnatwaL -j2 -m -k
GNATMAKE_FLAGS += $(GCOV_FLAGS)

ADA_SOURCE =					\
bc.ad[bs]					\
bc-containers.ad[bs]				\
bc-copy.ad[bs]					\
bc-filter.ad[bs]

ADA_SOURCE +=						\
bc-support-avl_trees.ad[bs]				\
bc-support-bounded.ad[bs]				\
bc-support-bounded_hash_tables.ad[bs]			\
bc-support-dynamic.ad[bs]				\
bc-support-exceptions.ad[bs]				\
bc-support-hash_tables.ad[bs]				\
bc-support-high_resolution_time.ad[bs]			\
bc-support-high_resolution_time-clock.adb-pentium	\
bc-support-high_resolution_time-clock.adb-ppc32		\
bc-support-managed_storage.ad[bs]			\
bc-support-memory_streams.ad[bs]			\
bc-support-standard_storage.ads				\
bc-support-statistics.ad[bs]				\
bc-support-synchronization.ad[bs]			\
bc-support-synchronization-debug.ad[bs]			\
bc-support-unbounded.ad[bs]				\
bc-support-unmanaged.ad[bs]				\
bc-support-unmanaged_storage.ad[bs]			\
bc-support.ad[bs]

ADA_SOURCE +=					\
bc-containers-bags-bounded.ad[bs]		\
bc-containers-bags-dynamic.ad[bs]		\
bc-containers-bags-unbounded.ad[bs]		\
bc-containers-bags-unmanaged.ad[bs]		\
bc-containers-bags.ad[bs]

ADA_SOURCE +=					\
bc-containers-collections-bounded.ad[bs]	\
bc-containers-collections-dynamic.ad[bs]	\
bc-containers-collections-unbounded.ad[bs]	\
bc-containers-collections-unmanaged.ad[bs]	\
bc-containers-collections.ad[bs]

ADA_SOURCE +=					\
bc-containers-deques-bounded.ad[bs]		\
bc-containers-deques-dynamic.ad[bs]		\
bc-containers-deques-unbounded.ad[bs]		\
bc-containers-deques-unmanaged.ad[bs]		\
bc-containers-deques.ad[bs]

ADA_SOURCE +=					\
bc-graphs.ad[bs]				\
bc-graphs-directed.ad[bs]			\
bc-graphs-undirected.ad[bs]

ADA_SOURCE +=					\
bc-lists-double.ad[bs]				\
bc-lists-single.ad[bs]				\
bc-lists.ad[bs]					\
bc-containers-lists-double.ad[bs]		\
bc-containers-lists-single.ad[bs]		\
bc-containers-lists.ad[bs]

ADA_SOURCE +=					\
bc-containers-maps-bounded.ad[bs]		\
bc-containers-maps-dynamic.ad[bs]		\
bc-containers-maps-unbounded.ad[bs]		\
bc-containers-maps-unmanaged.ad[bs]		\
bc-containers-maps.ad[bs]

ADA_SOURCE +=						\
bc-containers-collections-ordered-bounded.ad[bs]	\
bc-containers-collections-ordered-dynamic.ad[bs]	\
bc-containers-collections-ordered-unbounded.ad[bs]	\
bc-containers-collections-ordered-unmanaged.ad[bs]	\
bc-containers-collections-ordered.ad[bs]

ADA_SOURCE +=					\
bc-containers-queues-ordered.ad[bs]		\
bc-containers-queues-ordered-bounded.ad[bs]	\
bc-containers-queues-ordered-dynamic.ad[bs]	\
bc-containers-queues-ordered-unbounded.ad[bs]	\
bc-containers-queues-ordered-unmanaged.ad[bs]

ADA_SOURCE +=					\
bc-containers-queues-bounded.ad[bs]		\
bc-containers-queues-dynamic.ad[bs]		\
bc-containers-queues-unbounded.ad[bs]		\
bc-containers-queues-unmanaged.ad[bs]		\
bc-containers-queues.ad[bs]

ADA_SOURCE +=					\
bc-containers-rings.ad[bs]			\
bc-containers-rings-bounded.ad[bs]		\
bc-containers-rings-dynamic.ad[bs]		\
bc-containers-rings-unbounded.ad[bs]		\
bc-containers-rings-unmanaged.ad[bs]

ADA_SOURCE +=					\
bc-containers-sets-bounded.ad[bs]		\
bc-containers-sets-dynamic.ad[bs]		\
bc-containers-sets-unbounded.ad[bs]		\
bc-containers-sets-unmanaged.ad[bs]		\
bc-containers-sets.ad[bs]

ADA_SOURCE +=					\
bc-support-auto_pointers.ad[bs]			\
bc-support-smart_pointers.ad[bs]

ADA_SOURCE +=					\
bc-containers-quicksort.ad[bs]			\
bc-containers-shellsort.ad[bs]

ADA_SOURCE +=					\
bc-containers-stacks-bounded.ad[bs]		\
bc-containers-stacks-dynamic.ad[bs]		\
bc-containers-stacks-unbounded.ad[bs]		\
bc-containers-stacks-unmanaged.ad[bs]		\
bc-containers-stacks.ad[bs]

ADA_SOURCE +=					\
bc-trees-avl_trees.ad[bs]			\
bc-trees-avl_trees-print.ad[bs]			\
bc-trees-avl_trees-validate.ad[bs]		\
bc-trees-binary_trees.ad[bs]			\
bc-trees-binary_trees-in_order.ad[bs]		\
bc-trees-binary_trees-post_order.ad[bs]		\
bc-trees-binary_trees-pre_order.ad[bs]		\
bc-trees-multiway_trees.ad[bs]			\
bc-trees-multiway_trees-post_order.ad[bs]	\
bc-trees-multiway_trees-pre_order.ad[bs]	\
bc-trees.ad[bs]					\
bc-containers-trees-avl.ad[bs]			\
bc-containers-trees-avl-print.ad[bs]		\
bc-containers-trees-avl-validate.ad[bs]		\
bc-containers-trees-binary.ad[bs]		\
bc-containers-trees-binary-in_order.ad[bs]	\
bc-containers-trees-binary-post_order.ad[bs]	\
bc-containers-trees-binary-pre_order.ad[bs]	\
bc-containers-trees-multiway.ad[bs]		\
bc-containers-trees-multiway-post_order.ad[bs]	\
bc-containers-trees-multiway-pre_order.ad[bs]	\
bc-containers-trees.ad[bs]

ADA_SOURCE +=					\
bc-simple_collections.adb			\
bc-simple_collections.ads			\
bc-support-array_streams.adb			\
bc-support-array_streams.ads			\
bc-trees-avl_trees-iterators.adb		\
bc-trees-avl_trees-iterators.ads

ADA_SOURCE +=							\
bc-indefinite_containers.adb					\
bc-indefinite_containers.ads					\
bc-indefinite_containers-collections.adb			\
bc-indefinite_containers-collections.ads			\
bc-indefinite_containers-collections-bounded.adb		\
bc-indefinite_containers-collections-bounded.ads		\
bc-indefinite_containers-collections-dynamic.adb		\
bc-indefinite_containers-collections-dynamic.ads		\
bc-indefinite_containers-collections-ordered.ads		\
bc-indefinite_containers-collections-ordered-bounded.adb	\
bc-indefinite_containers-collections-ordered-bounded.ads	\
bc-indefinite_containers-collections-ordered-dynamic.adb	\
bc-indefinite_containers-collections-ordered-dynamic.ads	\
bc-indefinite_containers-collections-ordered-unbounded.adb	\
bc-indefinite_containers-collections-ordered-unbounded.ads	\
bc-indefinite_containers-collections-ordered-unmanaged.adb	\
bc-indefinite_containers-collections-ordered-unmanaged.ads	\
bc-indefinite_containers-collections-unbounded.adb		\
bc-indefinite_containers-collections-unbounded.ads		\
bc-indefinite_containers-collections-unmanaged.adb		\
bc-indefinite_containers-collections-unmanaged.ads		\
bc-indefinite_containers-indefinite_iterators.adb		\
bc-indefinite_containers-indefinite_iterators.ads		\
bc-indefinite_containers-queues.adb				\
bc-indefinite_containers-queues.ads				\
bc-indefinite_containers-queues-bounded.adb			\
bc-indefinite_containers-queues-bounded.ads			\
bc-indefinite_containers-queues-dynamic.adb			\
bc-indefinite_containers-queues-dynamic.ads			\
bc-indefinite_containers-queues-ordered.ads			\
bc-indefinite_containers-queues-ordered-bounded.adb		\
bc-indefinite_containers-queues-ordered-bounded.ads		\
bc-indefinite_containers-queues-ordered-dynamic.adb		\
bc-indefinite_containers-queues-ordered-dynamic.ads		\
bc-indefinite_containers-queues-ordered-unbounded.adb		\
bc-indefinite_containers-queues-ordered-unbounded.ads		\
bc-indefinite_containers-queues-ordered-unmanaged.adb		\
bc-indefinite_containers-queues-ordered-unmanaged.ads		\
bc-indefinite_containers-queues-unbounded.adb			\
bc-indefinite_containers-queues-unbounded.ads			\
bc-indefinite_containers-queues-unmanaged.adb			\
bc-indefinite_containers-queues-unmanaged.ads			\
bc-support-indefinite_bounded.adb				\
bc-support-indefinite_bounded.ads				\
bc-support-indefinite_dynamic.adb				\
bc-support-indefinite_dynamic.ads				\
bc-support-indefinite_reference.adb				\
bc-support-indefinite_reference.ads				\
bc-support-indefinite_unbounded.adb				\
bc-support-indefinite_unbounded.ads				\
bc-support-indefinite_unmanaged.adb				\
bc-support-indefinite_unmanaged.ads			    


############################
# Distribution construction

# Create the current date, in the form yyyymmdd. This certainly works
# in Ubuntu Linux 8.04 & Mac OS X.
DATE ?= $(shell date +%Y%m%d)$(SUBRELEASE)

DISTRIBUTION_FILES =				\
bc-$(DATE).tgz					\
bc-$(DATE).tar.bz2				\
bc-$(DATE).zip

ifneq ($(shell which 7za),)
  DISTRIBUTION_FILES += bc-$(DATE).7z
endif

dist: COPYING README $(DISTRIBUTION_FILES)
	-@rm -rf $@
	mkdir -p $@
	cp -p $^ $@/
	cp -pR ../contrib $@/

SOURCE = Makefile COPYING README $(ADA_SOURCE)

# bc-support-high_resolution_time-clock.adb is a copy of the
# appropriate architecture-specific implementation. Choose the
# more-popular Pentium version for the standard distribution.

bc-$(DATE): force
	-rm -rf $@
	mkdir -p $@/src
	tar cf - $(SOURCE) | (cd $@/src; tar xvf -)
	cp -p \
	  bc-support-high_resolution_time-clock.adb-pentium \
	  bc-support-high_resolution_time-clock.adb-ppc32   \
	  $@/src/
	cp -p \
	  bc-support-high_resolution_time-clock.adb-pentium \
	  $@/src/bc-support-high_resolution_time-clock.adb
	mkdir $@/GNAT 
	cp -p \
	    ../GNAT/Makefile		   \
	    ../GNAT/booch_95.gpr	   \
	    ../GNAT/gnat-booch.sh	   \
	    ../GNAT/gnat-booch.conf	   \
	    $@/GNAT/
	$(MAKE) DIST=$(PWD)/$@ -C ../tests dist
	$(MAKE) DIST=$(PWD)/$@ -C ../demos dist

bc-$(DATE).tgz: bc-$(DATE)
	tar zcvf $@ $</

bc-$(DATE).tar.bz2: bc-$(DATE)
	tar --create --verbose --bzip2 --file=$@ $</

bc-$(DATE).zip: bc-$(DATE)
	zip -lr $@ $</*

bc-$(DATE).7z: bc-$(DATE)
	7za a -r $@ $</


##############
# Upload to SF

ifeq (${USER},martin)
    SFUSER	   ?= krischik
else ifeq (${USER},simon)
    SFUSER	   ?= simonjwright
else
    SFUSER         ?= nobody
endif

RSYNC_OPTS  =			\
	--compress		\
	--copy-unsafe-links	\
	--cvs-exclude		\
	--perms			\
	--recursive		\
	--rsh=ssh		\
	--times			\
	--update		\
	--verbose

upload-docs: force
	rsync						\
	  $(RSYNC_OPTS)					\
	  ../html/*.{html,gif,jpg,png} 			\
	  ../html/x.ada					\
	  ../html/coldframe-hash.{tgz,zip}		\
	  $(SFUSER),booch95@web.sourceforge.net:htdocs/

upload-dist: force
	rsync						\
	  $(RSYNC_OPTS)					\
	  $(DISTRIBUTION_FILES)				\
	  "$(SFUSER)@frs.sourceforge.net:uploads/" 


.PHONY: force

# vim: textwidth=0 nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab
# vim: filetype=make encoding=utf-8 fileformat=unix
