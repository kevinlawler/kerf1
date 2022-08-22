PREFIX = /usr/local
CFLAGS =$(ARGS)#for eg: make ARGS=-DKEVIN #trigger user-specific compilation
CFLAGS += -Qunused-arguments #really we should just fix it so that -rdynamic (& -pie ?)  only happens at link time
LDFLAGS=
PRODFLAGS = -Os
CC = clang
# LIB=libkerf?
DEVFLAGS  = -O0 -g -DDEBUG 
MAKEFLAGS += --jobs=$(CPUS) #parallelize

BASEDIR = ./
OBJDIR = $(BASEDIR)/obj/

#Add the -g symbol debugging flag if the DEBUG flag is previously defined
#This is useful for say seeing line numbers in the lldb/gdb debuggers
#Example use: to add -g to release version from the command line: make kerf ARGS="-DDEBUG"
ifneq (,$(findstring DDEBUG, $(CFLAGS)))
  $(info Adding -g debug flag)
  CFLAGS += -g
endif

ifdef STATIC
  CFLAGS += -static
  LDFLAGS += -lfts
endif

$(shell mkdir -p "$(OBJDIR)")

GLOBAL_HEADERS = kerf.h dry.h help.h

SOURCES = alter.c  cow.c  emu.c grade.c inet.c lex.c nonce.c  puts.c  stamp.c  \
wire.c  zip.c break.c func.c hash.c jump.c  main.c opt.c query.c \
table.c  verb-dispatch.c verb-regular.c verb-math.c xml.c unit.c api.c lz4.c

#OBJS  = $(patsubst %.c, %.o, $(SOURCES))

#how do we make this work? to get .o files in a subdirectory?
#OBJS := $(addprefix, $(OBJDIR), $(OBJS))
OBJS := $(addprefix $(OBJDIR), $(SOURCES:.c=.o))

OS := $(shell uname -s | tr "[:upper:]" "[:lower:]")

ifeq (linux,$(OS))
# x-platform linux version can use static libedit lib, if compiling for self use only, can instead link -ledit 
# LDFLAGS +=  -ldl -lm -lpthread -lrt -ltermcap -lz ./libedit.a
LDFLAGS +=  -ldl -lm -lpthread -lrt -ltermcap -lz -ledit
CFLAGS  += -rdynamic -m64 # -w
CFLAGS += -fcommon
ifneq (,$(findstring DDEBUG, $(CFLAGS)))
  $(info Adding -g debug flag)
  CFLAGS  += -fPIC # -fPIE -pie 
endif
ifeq (,$(findstring DDEBUG, $(CFLAGS)))
  CFLAGS += -fPIE -pie #necessary for loading executable via rdynamic
endif

CPUS ?= $(shell nproc)
endif

ifeq (darwin,$(OS))
##################################################################################
##To Use OSX `homebrew` clang not Xcode clang
##brew install llvm --with-clang
#CC = /usr/local/opt/llvm/bin/clang
#LDFLAGS += -L/usr/local/opt/llvm/lib
#CFLAGS  += -I/usr/local/opt/llvm/include
##################################################################################
#PRODFLAGS += -flto -fuse-linker-plugin #alternative: compile all .c files at once
LDFLAGS += -ledit -lz
CFLAGS  += -rdynamic -m64
CFLAGS  += -framework Accelerate
CPUS ?= $(shell sysctl -n hw.ncpu || echo 8)
endif

# ifeq (freebsd,$(OS))
# endif

# ifeq (openbsd,$(OS))
# endif

# ifeq (cygwin_nt-6.3,$(OS))
# LDFLAGS = -ledit
# endif

# ifeq (sunos,$(OS))
# LDFLAGS += -lsocket
# PRODFLAGS += -fast
# endif

# kerf_test versions of OBJS

OBJS_T= $(shell echo ${OBJS} | sed -e "s/\.o/.t.o/g")

all: kerf kerf_test

test:  kerf_test
debug: test #alias

release: kerf

# lib: $(LIB)

$(OBJS): $(OBJDIR)%.o: $(BASEDIR)%.c $(GLOBAL_HEADERS)
	 $(CC) ${CFLAGS} -c $< -o $@



kerf: CFLAGS += $(PRODFLAGS)
kerf: $(OBJS)
	$(CC) ${CFLAGS} $^ -o $@ $(LDFLAGS)

kerf_test: CFLAGS += $(DEVFLAGS)

$(OBJS_T): $(OBJDIR)%.t.o: $(BASEDIR)%.c $(GLOBAL_HEADERS)
	 $(CC) ${CFLAGS} -c $< -o $@

kerf_test: $(OBJS_T)
	$(CC) ${CFLAGS} $^ -o $@ $(LDFLAGS)

install:
	install kerf $(PREFIX)/bin/kerf


clean: cleandocs cleandata
	$(RM) -r kerf kerf_test $(OBJDIR)*.o 

cleandata:
	$(RM) -r scripts/test/stripes/datadir/

wire-test: 
	echo "running kerf -p 1234" 
	./kerf -p 1234  1>/dev/null 2>/dev/null&
	sleep 1
	./kerf_test scripts/test/wire/wire-unit.kerf silent
	./kerf_test scripts/test/wire/wire-loader.kerf silent 

fork-wire-test:
	echo "run kerf_test -F -p 5555"
	./kerf_test scripts/test/wire/wire-sleep.kerf

stripe-test: cleandata
	echo "should exit clean"
	./kerf_test scripts/test/stripes/stripe-test.kerf silent 2>/dev/null

upsert-test: cleandata
	echo "should exit clean"
	./kerf scripts/test/upsert/upsert-test.kerf silent 2>/dev/null

stripe-test-verbose: cleandata
	echo "should exit clean; will be very verbose"
	./kerf_test scripts/test/stripes/stripe-test.kerf loud 2>/dev/null

csv-test:
	./kerf_test scripts/test/csv/csv-test.kerf silent 2>/dev/null

bar-test:
	./kerf_test scripts/test/bars/bartest.kerf silent 2>/dev/null

atlas-test:
	./kerf_test scripts/test/atlas/atlas-test.kerf silent 2>/dev/null

cleanparcel:
	$(RM) -r scripts/test/parcel/datadir/

parcel-data: cleanparcel
	./kerf scripts/test/parcel/parcel-create.kerf

load-parcel-data:
	./kerf scripts/test/parcel/parcel-load.kerf


#TAGS: *.c *.h
#	etags *.[ch]

%.t.o: %.c
	$(CC) $(CFLAGS) -c $(CPPFLAGS) -o $@ $<

docs: ../manual/refmanual.tex 
	pdflatex --output-directory ../manual ../manual/refmanual.tex 
	pdflatex --output-directory ../manual ../manual/refmanual.tex 
	pdflatex --output-directory ../manual ../manual/refcard.tex 
	pdflatex --output-directory ../manual ../manual/refcard.tex 
	pdflatex --output-directory ../manual ../manual/kerfdb.tex 
	pdflatex --output-directory ../manual ../manual/kerfdb.tex 

cleandocs:
	find ../manual/ -type f -not -name '*.tex' -print0 | xargs -0 -I {} rm {}

.PHONY: all clean install 


# DO NOT DELETE
