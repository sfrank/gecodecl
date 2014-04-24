#LOCATION = /home/sfrank/prj/g
OPTIONS = -ggdb -L$(LOCATION) -I$(LOCATION)
WARNINGS = -W -Wall -Wextra
LIBS = -lgecodesearch -lgecodekernel -lgecodeint -lgecodefloat -lgecodeset -lgecodeminimodel -lgecodesupport -lstdc++
#OPTIONS = -O3

all: libgecodeglue.so gecodeglue.lisp

libgecodeglue.so: gecodeglue.h gecodeglue.cpp
	g++ $(OPTIONS) $(WARNINGS) -fPIC -shared $(LIBS) -o libgecodeglue.so gecodeglue.cpp

gecodeglue.lisp: gecodeglue.swig gecodeglue.h libgecodeglue.so
	swig -cffi -noswig-lisp gecodeglue.swig

test: test.o
	g++ $(OPTIONS) test.o $(LIBS) -o test

test.o: gecodeglue.cpp
	g++ $(OPTIONS) -c -Wall -Wextra -o test.o gecodeglue.cpp
