PROJECT = test_utf8
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -DDEBUG
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../src/curl_request_maker -I../../include/utfcpp -I../../src/twitter_api -I../../src/script_detector

LINK_DIR = -L../../lib

LIBS = -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

BINS = ${BIN_DIR}/t_encode \
       ${BIN_DIR}/t_test_utfcpp

all: ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS}
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_encode: t_encode.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_encode
	@echo "done!"

${BIN_DIR}/t_test_utfcpp: t_test_utfcpp.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_test_utfcpp
	@echo "done!"

# DO NOT DELETE THIS LINE
