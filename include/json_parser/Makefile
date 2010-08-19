PROJECT = jsonparser
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/

LINK_DIR = -L../../lib

LIBS = -l${PROJECT}

LIB_DIR = ../../lib
BIN_DIR = ../../bin


LIB = ${LIB_DIR}/lib${PROJECT}.a

BINS = ${BIN_DIR}/t_json_parser

OBJS = JSON.o JSONValue.o

all: ${LIB} ${BINS}
	@echo "build done!"

lib: ${LIB}
	@echo "lib done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cpp.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_json_parser: t_json_parser.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_json_parser
	@echo "done!"

# DO NOT DELETE THIS LINE
