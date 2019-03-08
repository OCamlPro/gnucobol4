#
# This Makefile documents how fileio.c was split up: what tools were
# used and what process.  It's here in case it's needed again after a
# future merge.
#
# This file is int the public domain.
#

.SUFFIXES: .dat .load

CPPFLAGS =  -DHAVE_CONFIG_H  \
	-D_POSIX_C_SOURCE=200112L -D_BSD_SOURCE \
	-I $(dir $(PWD)) -I $(shell dirname $(dir $(PWD))) \
	-I../../build \
	-I/usr/local/include/libxml2 

CFLAGS = -g -O0 --std=c11 \
	-Wswitch \
	-Werror \
	-Wall \
	-Wpointer-arith \
	-Wno-unused-function \
	-Wno-unused-value \
	-Wno-missing-braces \
	-Wno-parentheses \
	-Wno-unused-result \
	-Wno-strict-aliasing \
	-pipe \
	-fsigned-char \
	-Wall \
	-Wwrite-strings 

SRC = $(wildcard *.c)
OBJ = $(subst .c,.o,$(SRC))

all: bdb.o lmdb.o util.o # $(OBJ)

include:
	cpp $(CPPFLAGS) -v $(SRC)

split: cisam.c disam.c vbisam.c util.c # extfh.c

# https://pypi.org/project/pcpp/#files
# python3 setup.py install
PCPP = pcpp --passthru-defines \
	    --passthru-unfound-includes \
	    --passthru-comments

#ILEIO.C = ../../../../trunk/libcob/fileio.c
#ILEIO.C = ../orig/fileio.c
FILEIO.C = ../fileio.c
NEVER = -N WITH_LMDB -N WITH_DB -N WITH_BDB_OR_MDB
NOISAM = $(addprefix -N WITH_,$(addsuffix ISAM,C D VB ANY_))

cisam.c: $(FILEIO.C)
	$(PCPP) $(NEVER) -D WITH_CISAM -D WITH_ANY_ISAM -o$@ $^ 
disam.c: $(FILEIO.C)
	$(PCPP) $(NEVER) -D WITH_DISAM -D WITH_ANY_ISAM -o$@ $^ 
vbisam-split.c: $(FILEIO.C)
	$(PCPP) $(NEVER) -D WITH_VBISAM -D WITH_ANY_ISAM -o$@ $^ 
util.c: $(FILEIO.C)
	$(PCPP) $(NEVER) $(NOISAM) -o$@ $^ 

DAT = $(addsuffix isam.dat,c d vb)

dat: $(DAT)

bdb.o: bdb.c isam.h 
	$(CC) -c -o$@ $(CPPFLAGS) -D WITH_DB $(CFLAGS) $< $(LDFLAGS)
lmdb.o: lmdb.c isam.h
	$(CC) -c -o$@ $(CPPFLAGS) -D WITH_LMDB $(CFLAGS) $< $(LDFLAGS)
xisam.o: xisam.c isam.h xisam.h
	$(CC) -c -o$@ $(CPPFLAGS) -D WITH_VBISAM $(CFLAGS) $< $(LDFLAGS)

## nm2dat: 
##  #! /bin/sh
##  set -e
##  
##  if [ ! "$1" ]
##  then
##      #cho syntax: $0 [-o output] objfile >&2
##      echo syntax: $0 [-C] objfile >&2
##      exit 1
##  fi
##  
##  while getopts o: opt
##  do
##      case $opt in 
##  	C) use_stdout=y
##      esac
##  done
##  
##  shift $(($OPTIND - 1))
##  
##  while test "$1"
##  do
##      if [ "$use_stdout" ]
##      then
##  	output=/dev/stdout
##      else
##  	output=${1%.*}.dat
##      fi
##      
##      nm $1 | awk -vname=${1#*/} \
##  		'NF == 3 { OFS = "\t"; print name, $2, $3 }' > $output
##      shift
##  done

.o.dat:
	nm2dat $^

LOAD = $(addsuffix .load,fileio bdb lmdb xisam)

load: $(LOAD)


# SQLite can be used to look for
# * static-static duplicates between files
# * public-public conflicts
# * static functions hiding public ones
# * missing static functions between implementation files

db:
	sqlite3 db " \
		create table if not exists nm ( \
			file text not NULL, \
			type text not NULL, \
			symbol text not NULL, \
			primary key (file, symbol) \
		); "

.dat.load:
	sqlite3 db "delete from nm where file = '$(subst .dat,.o,$^)'"
	sqlite3 -separator '	' db ".import '$^' nm"
