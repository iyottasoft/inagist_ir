PROJECT = curlrequestmaker
CXX = g++
CXXFLAGS = -Wall -g
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lcurl -ljsonparser

LIB_DIR = ../../lib
BIN_DIR = ../../bin


LIB = ${LIB_DIR}/lib${PROJECT}.a

BINS = ${BIN_DIR}/t_curl_request_maker

OBJS = curl_request_maker.o

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

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_curl_request_maker: t_curl_request_maker.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_curl_request_maker
	@echo "done!"

# DO NOT DELETE THIS LINE
