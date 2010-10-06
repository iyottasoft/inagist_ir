PROJECT = utfcpp 
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/

LINK_DIR = -L../../lib

LIBS = -l${PROJECT}

LIB_DIR = ../../lib
BIN_DIR = ../../bin

BINS = ${BIN_DIR}/t_test_utfcpp \

all: ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning binaries ..."
	@rm ${BINS}
	@echo "done!"

.cpp.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_test_utfcpp: t_test_utfcpp.cc
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} -o ${BIN_DIR}/t_test_utfcpp
	@echo "done!"

# DO NOT DELETE THIS LINE
