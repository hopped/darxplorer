############################################################################
#  Copyright (C) 2005  Martin Krischik
#
#  This library is free software; you can redistribute it and/or
#  modify it under terms of the GNU General Public License as
#  published by the Free Software Foundation; either version 2, or
#  (at your option) any later version. This package is distributed in
#  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
#  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#  PARTICULAR PURPOSE. See the GNU General Public License for more
#  details. You should have received a copy of the GNU General Public
#  License distributed with this package; see file COPYING.  If not,
#  write to the Free Software Foundation, 59 Temple Place - Suite
#  330, Boston, MA 02111-1307, USA.
############################################################################
#  CVS Daten:
#
#  $Author: krischik $
#
#  $Revision: 1395 $
#  $Date: 2009-02-16 08:18:23 +0000 (Mon, 16 Feb 2009) $
#
#  $Id makefile,v 4.17 2004/02/23 13:08:49 krischik Exp $
#  $HeadURL: https://booch95.svn.sourceforge.net/svnroot/booch95/trunk/GNAT/Makefile $
#
############################################################################

.PHONY		: all					#  make all
.PHONY		: debug					#  make debug
.PHONY		: release				#  make release
.PHONY		: clean					#  cleanup
.PHONY		: pretty				#  pretty printer
.PHONY		: booch_95				#  make library only

.SUFFIXES	: .ada .adb .ads

vpath %.ads	 ../src
vpath %.adb	 ../src
vpath %.ada	 ../src

ifndef GPR_OS
	#
	#  Try to guess GPR settings when they are not already set.
	#
	#  Note 1: If you set GPR_OS you need to set the other variables 
	#	   mentioned here as well.
	#
	#  Note 2: The GNU Ada Project is supplied with a more advanced 
	#	   detector which supports more combinations - including
	#	   cross compilations.
	#

	ifeq (${OS},Windows_NT)
		GPR_OS		:= Windows_NT
		GPR_CPU		:= i586
		GPR_Target	:= native
		MKDIR		:= /bin/mkdir --verbose --parents
		DELETE		:= /usr/bin/rm --verbose
		GNAT		:= gnat
	endif

	ifdef sys$login
		GPR_OS		:= VMS 
		GPR_CPU		:= APX
		GPR_Target	:= native
		GNAT		:= gnat
		MKDIR		:= CREATE Directory /Log
		DELETE		:= DELETE /Log
	endif

	ifdef OS2_SHELL
		GPR_OS		:= OS2
		GPR_CPU		:= i586
		GPR_Target	:= native
		MKDIR		:= mkdir
		DELETE		:= rm --verbose
		GNAT		:= gnat
	endif

	ifeq ($(shell uname), Linux)
		GPR_OS		:= $(shell uname --kernel-name)
		GPR_CPU		:= $(shell uname --machine)
		GPR_Target	:= native
		MKDIR		:= mkdir --verbose --parents
		DELETE		:= rm --verbose
		GNAT		:= gnat
	endif

	ifeq ($(shell uname), SunOS)
		GPR_CPU		:= $(shell uname --processor)
	endif

	ifeq ($(shell uname), Darwin)
		GPR_OS		:= Darwin
		GPR_CPU		:= $(shell uname -p)
		GPR_Target	:= native
		ifeq ($(GPR_CPU),powerpc)
			GPR_CPU	:= ppc32
		endif
		ifeq ($(GPR_CPU),i386)
			GPR_CPU	:= i686
		endif
		MKDIR		:= mkdir -v -p
		DELETE		:= rm -v
		GNAT		:= gnat
	endif
else
	MKDIR		:= mkdir --verbose --parents
	DELETE		:= rm --verbose
endif

############################################################################
#                                                                          #
#  Set working directories                                                 #
#                                                                          #
############################################################################

Base_Dir				:= ${CURDIR}

ifeq (${GPR_Target},native)
	Debug_Dir			:= ${Base_Dir}/${GPR_OS}-${GPR_CPU}-Debug
	Release_Dir			:= ${Base_Dir}/${GPR_OS}-${GPR_CPU}-Release
else
	Debug_Dir			:= ${Base_Dir}/${GPR_Target}-${GPR_CPU}-Debug
	Release_Dir			:= ${Base_Dir}/${GPR_Target}-${GPR_CPU}-Release
endif

GNAT_Opt				:= "-XGPR_CPU=${GPR_CPU}" "-XGPR_OS=${GPR_OS}"
GNAT_Opt_Debug			:= ${GNAT_Opt} "-Xstyle=Debug"
GNAT_Opt_Release		:= ${GNAT_Opt} "-Xstyle=Release"

all:															\
		booch_95												\

debug:															\
		${Debug_Dir}											\
		${Debug_Dir}/lib										\
		${Debug_Dir}/obj
	gnat make ${GNAT_Opt_Debug} "-P" booch_95.gpr

release:														\
		${Release_Dir}											\
		${Release_Dir}/lib										\
		${Release_Dir}/obj
	gnat make ${GNAT_Opt_Release} "-P" booch_95.gpr

clean:
	gnat clean ${GNAT_Opt_Debug}   "-P" booch_95.gpr
	gnat clean ${GNAT_Opt_Release} "-P" booch_95.gpr

pretty:
	-gnat pretty ${GNAT_Opt} "-P" booch_95.gpr

booch_95:														\
		${Debug_Dir}											\
		${Debug_Dir}/lib										\
		${Debug_Dir}/obj										\
		${Release_Dir}											\
		${Release_Dir}/lib										\
		${Release_Dir}/obj
	gnat make ${GNAT_Opt_Release} "-P" booch_95.gpr
	gnat make ${GNAT_Opt_Debug}   "-P" booch_95.gpr

############################################################################
#                                                                          #
#  Make Directories                                                        #
#                                                                          #
############################################################################

${Debug_Dir}:
	-${MKDIR} "${Debug_Dir}"

${Debug_Dir}/lib:
	-${MKDIR} "${Debug_Dir}/lib"

${Debug_Dir}/obj:
	-${MKDIR} "${Debug_Dir}/obj"

${Release_Dir}:
	-${MKDIR} "${Release_Dir}"

${Release_Dir}/lib:
	-${MKDIR} "${Release_Dir}/lib"

${Release_Dir}/obj:
	-${MKDIR} "${Release_Dir}/obj"

############################################################################
#                                                                          #
#  Create Booch Componenets for Indefinite Items                           #
#                                                                          #
############################################################################

BC_Generated_Files :=													\
		../bc-support-indefinite_dynamic.ads							\
		../bc-support-indefinite_dynamic.adb							\
		../bc-support-indefinite_unbounded.ads							\
		../bc-support-indefinite_unbounded.adb							\
		../bc-support-indefinite_bounded.ads							\
		../bc-support-indefinite_bounded.adb							\
		../bc-support-indefinite_unmanaged.ads							\
		../bc-support-indefinite_unmanaged.adb							\
		../bc-indefinite_containers.ads									\
		../bc-indefinite_containers.adb									\
		../bc-indefinite_containers-queues.ads							\
		../bc-indefinite_containers-queues.adb							\
		../bc-indefinite_containers-queues-dynamic.ads					\
		../bc-indefinite_containers-queues-dynamic.adb					\
		../bc-indefinite_containers-queues-bounded.ads					\
		../bc-indefinite_containers-queues-bounded.adb					\
		../bc-indefinite_containers-queues-unbounded.ads				\
		../bc-indefinite_containers-queues-unbounded.adb				\
		../bc-indefinite_containers-queues-unmanaged.ads				\
		../bc-indefinite_containers-queues-unmanaged.adb				\
		../bc-indefinite_containers-queues-ordered.ads					\
		../bc-indefinite_containers-queues-ordered-dynamic.ads			\
		../bc-indefinite_containers-queues-ordered-dynamic.adb			\
		../bc-indefinite_containers-queues-ordered-bounded.ads			\
		../bc-indefinite_containers-queues-ordered-bounded.adb			\
		../bc-indefinite_containers-queues-ordered-unbounded.ads		\
		../bc-indefinite_containers-queues-ordered-unbounded.adb		\
		../bc-indefinite_containers-queues-ordered-unmanaged.ads		\
		../bc-indefinite_containers-queues-ordered-unmanaged.adb		\
		../bc-indefinite_containers-collections.ads						\
		../bc-indefinite_containers-collections.adb						\
		../bc-indefinite_containers-collections-dynamic.ads				\
		../bc-indefinite_containers-collections-dynamic.adb				\
		../bc-indefinite_containers-collections-bounded.ads				\
		../bc-indefinite_containers-collections-bounded.adb				\
		../bc-indefinite_containers-collections-unbounded.ads			\
		../bc-indefinite_containers-collections-unbounded.adb			\
		../bc-indefinite_containers-collections-unmanaged.ads			\
		../bc-indefinite_containers-collections-unmanaged.adb			\
		../bc-indefinite_containers-collections-ordered.ads				\
		../bc-indefinite_containers-collections-ordered-dynamic.ads		\
		../bc-indefinite_containers-collections-ordered-dynamic.adb		\
		../bc-indefinite_containers-collections-ordered-bounded.ads		\
		../bc-indefinite_containers-collections-ordered-bounded.adb		\
		../bc-indefinite_containers-collections-ordered-unmanaged.ads	\
		../bc-indefinite_containers-collections-ordered-unmanaged.adb

#		../bc-support-indefinite_bounded_hash_tables.adb				\
#		../bc-support-indefinite_bounded_hash_tables.ads				\
#		../bc-indefinite_containers-bags.ads							\
#		../bc-indefinite_containers-bags.adb							\
#		../bc-indefinite_containers-bags-dynamic.adb					\
#		../bc-indefinite_containers-bags-bounded.ads					\
#		../bc-indefinite_containers-bags-bounded.adb					\
#		../bc-indefinite_containers-bags-unbounded.ads					\
#		../bc-indefinite_containers-bags-unbounded.adb					\
#		../bc-indefinite_containers-bags-unmanaged.ads					\
#		../bc-indefinite_containers-bags-unmanaged.adb					\

generate:																\
		${BC_Generated_Files}

clean_generate:
	${DELETE} ${BC_Generated_Files}

########### Support

../bc-support-indefinite_dynamic.ads: \
		../bc-support-dynamic.ads
	sarBC --verbose --base --item_without_in --dynamic --dynamic_support --out="$(@)" "$(<)"

../bc-support-indefinite_dynamic.adb: \
			../bc-support-dynamic.adb
	sarBC --verbose --base --item_without_in --dynamic --dynamic_support --out="$(@)" "$(<)"

../bc-support-indefinite_unbounded.ads: \
		../bc-support-unbounded.ads
	sarBC --verbose --base --unbounded --unbounded_support --item_without_in --out="$(@)" "$(<)"

../bc-support-indefinite_unbounded.adb: \
		../bc-support-unbounded.adb
	sarBC --verbose --base --unbounded --unbounded_support --item_without_in --out="$(@)" "$(<)"

../bc-support-indefinite_bounded.ads: \
		../bc-support-bounded.ads
	sarBC --verbose --base --bounded --bounded_support --item_without_in --out="$(@)" "$(<)"

../bc-support-indefinite_bounded.adb: \
		../bc-support-bounded.adb
	sarBC --verbose --base --bounded --bounded_support --item_without_in --out="$(@)" "$(<)"

../bc-support-indefinite_unmanaged.ads: \
		../bc-support-unmanaged.ads
	sarBC --verbose --base --unmanaged --unmanaged_support --item_without_in --out="$(@)" "$(<)"

../bc-support-indefinite_unmanaged.adb: \
		../bc-support-unmanaged.adb
	sarBC --verbose --base --unmanaged --unmanaged_support --item_without_in --out="$(@)" "$(<)"

../bc-support-indefinite_bounded_hash_tables.ads: \
		../bc-support-bounded_hash_tables.ads
	sarBC --verbose --base --bounded --bounded_support --item_without_in --out="$(@)" "$(<)"

../bc-support-indefinite_bounded_hash_tables.adb: \
		../bc-support-bounded_hash_tables.adb
	sarBC --verbose --base --bounded --bounded_support --item_without_in --out="$(@)" "$(<)"

########### Containers

../bc-indefinite_containers.ads: \
		../bc-containers.ads
	sarBC --verbose --base --storage --out="$(@)" "$(<)"

../bc-indefinite_containers.adb: \
		../bc-containers.adb
	sarBC --verbose --base --storage --out="$(@)" "$(<)"

########### Bags

../bc-indefinite_containers-bags.ads: \
		../bc-containers-bags.ads
	sarBC --verbose --base --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-bags.adb: \
		../bc-containers-bags.adb
	sarBC --verbose --base --item_without_in --compare_current_item --out="$(@)" "$(<)"

../bc-indefinite_containers-bags-dynamic.ads: \
		../bc-containers-bags-dynamic.ads
	sarBC --verbose --base --item_without_in --dynamic --out="$(@)" "$(<)"

../bc-indefinite_containers-bags-dynamic.adb: \
		../bc-containers-bags-dynamic.adb
	sarBC --verbose --base --item_without_in --dynamic --out="$(@)" "$(<)"

../bc-indefinite_containers-bags-bounded.ads: \
		../bc-containers-bags-bounded.ads
	sarBC --verbose --base --item_without_in --bounded --out="$(@)" "$(<)"

../bc-indefinite_containers-bags-bounded.adb: \
			../bc-containers-bags-bounded.adb
	sarBC --verbose --base --item_without_in --bounded --out="$(@)" "$(<)"

../bc-indefinite_containers-bags-unbounded.ads: \
		../bc-containers-bags-unbounded.ads
	sarBC --verbose --base --item_without_in --unbounded --out="$(@)" "$(<)"

../bc-indefinite_containers-bags-unbounded.adb: \
		../bc-containers-bags-unbounded.adb
	sarBC --verbose --base --item_without_in --unbounded --out="$(@)" "$(<)"

../bc-indefinite_containers-bags-unmanaged.ads: \
		../bc-containers-bags-unmanaged.ads
	sarBC --verbose --base --item_without_in --unmanaged --out="$(@)" "$(<)"

../bc-indefinite_containers-bags-unmanaged.adb: \
		../bc-containers-bags-unmanaged.adb
	sarBC --verbose --base --item_without_in --unmanaged --out="$(@)" "$(<)"

########### Collections

../bc-indefinite_containers-collections.ads: \
		../bc-containers-collections.ads
	sarBC --verbose --base --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-collections.adb: \
		../bc-containers-collections.adb
	sarBC --verbose --base --item_without_in --compare_current_item --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-dynamic.ads: \
		../bc-containers-collections-dynamic.ads
	sarBC --verbose --base --item_without_in --dynamic --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-dynamic.adb: \
		../bc-containers-collections-dynamic.adb
	sarBC --verbose --base --item_without_in --dynamic --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-bounded.ads: \
		../bc-containers-collections-bounded.ads
	sarBC --verbose --base --item_without_in --bounded --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-bounded.adb: \
			../bc-containers-collections-bounded.adb
	sarBC --verbose --base --item_without_in --bounded --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-unmanaged.ads: \
		../bc-containers-collections-unmanaged.ads
	sarBC --verbose --base --item_without_in --unmanaged --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-unmanaged.adb: \
		../bc-containers-collections-unmanaged.adb
	sarBC --verbose --base --item_without_in --unmanaged --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-unbounded.ads: \
		../bc-containers-collections-unbounded.ads
	sarBC --verbose --base --item_without_in --unbounded --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-unbounded.adb: \
		../bc-containers-collections-unbounded.adb
	sarBC --verbose --base --item_without_in --unbounded --out="$(@)" "$(<)"

########### ordered Collections

../bc-indefinite_containers-collections-ordered.ads: \
		../bc-containers-collections-ordered.ads
	sarBC --verbose --base --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-ordered-dynamic.ads: \
		../bc-containers-collections-ordered-dynamic.ads
	sarBC --verbose --base --item_without_in --dynamic --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-ordered-dynamic.adb: \
		../bc-containers-collections-ordered-dynamic.adb
	sarBC --verbose --base --item_without_in --dynamic --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-ordered-bounded.ads: \
		../bc-containers-collections-ordered-bounded.ads
	sarBC --verbose --base --item_without_in --bounded --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-ordered-bounded.adb: \
		../bc-containers-collections-ordered-bounded.adb
	sarBC --verbose --base --item_without_in --bounded --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-ordered-unbounded.ads: \
		../bc-containers-collections-ordered-unbounded.ads
	sarBC --verbose --base --item_without_in --unbounded --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-ordered-unbounded.adb: \
		../bc-containers-collections-ordered-unbounded.adb
	sarBC --verbose --base --item_without_in --unbounded --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-ordered-unmanaged.ads: \
		../bc-containers-collections-ordered-unmanaged.ads
	sarBC --verbose --base --item_without_in --unmanaged --out="$(@)" "$(<)"

../bc-indefinite_containers-collections-ordered-unmanaged.adb: \
		../bc-containers-collections-ordered-unmanaged.adb
	sarBC --verbose --base --item_without_in --unmanaged --out="$(@)" "$(<)"

########### Queues

../bc-indefinite_containers-queues.ads: \
		../bc-containers-queues.ads
	sarBC --verbose --base --bounded --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues.adb: \
		../bc-containers-queues.adb
	sarBC --verbose --base --bounded --item_without_in --compare_current_item --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-bounded.ads: \
		../bc-containers-queues-bounded.ads
	sarBC --verbose --base --bounded --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-bounded.adb: \
		../bc-containers-queues-bounded.adb
	sarBC --verbose --base --bounded --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-dynamic.ads: \
		../bc-containers-queues-dynamic.ads
	sarBC --verbose --base --dynamic --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-dynamic.adb: \
		../bc-containers-queues-dynamic.adb
	sarBC --verbose --base --dynamic --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-unbounded.ads: \
		../bc-containers-queues-unbounded.ads
	sarBC --verbose --base --unbounded --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-unbounded.adb: \
		../bc-containers-queues-unbounded.adb
	sarBC --verbose --base --unbounded --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-unmanaged.ads: \
		../bc-containers-queues-unmanaged.ads
	sarBC --verbose --base --unmanaged --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-unmanaged.adb: \
		../bc-containers-queues-unmanaged.adb
	sarBC --verbose --base --unmanaged --item_without_in --out="$(@)" "$(<)"

########### Ordered Queues

../bc-indefinite_containers-queues-ordered.ads: \
		../bc-containers-queues-ordered.ads
	sarBC --verbose --base --bounded --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-ordered-bounded.ads: \
		../bc-containers-queues-ordered-bounded.ads
	sarBC --verbose --base --bounded --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-ordered-bounded.adb: \
		../bc-containers-queues-ordered-bounded.adb
	sarBC --verbose --base --bounded --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-ordered-dynamic.ads: \
		../bc-containers-queues-ordered-dynamic.ads
	sarBC --verbose --base --dynamic --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-ordered-dynamic.adb: \
		../bc-containers-queues-ordered-dynamic.adb
	sarBC --verbose --base --dynamic --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-ordered-unbounded.ads: \
		../bc-containers-queues-ordered-unbounded.ads
	sarBC --verbose --base --unbounded --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-ordered-unbounded.adb: \
		../bc-containers-queues-ordered-unbounded.adb
	sarBC --verbose --base --unbounded --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-ordered-unmanaged.ads: \
		../bc-containers-queues-ordered-unmanaged.ads
	sarBC --verbose --base --unmanaged --item_without_in --out="$(@)" "$(<)"

../bc-indefinite_containers-queues-ordered-unmanaged.adb: \
		../bc-containers-queues-ordered-unmanaged.adb
	sarBC --verbose --base --unmanaged --item_without_in --out="$(@)" "$(<)"

############################################################################
# vim: textwidth=0 nowrap tabstop=4 shiftwidth=4 softtabstop=4 noexpandtab
# vim: filetype=make encoding=utf-8 fileformat=unix
