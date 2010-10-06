PROJECT = test_utf8
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -DDEBUG
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/stemmer/ -I../../include/json_parser -I../../src/curl_request_maker -I../../src/keywords_extracter -I../../include/utfcpp

LINK_DIR = -L../../lib

LIBS = -lstemmer -ljsonparser -lcurlrequestmaker -lcurl -lkeywordsextracter

LIB_DIR = ../../lib
BIN_DIR = ../../bin

BINS = ${BIN_DIR}/t_test_inagist_utf8 \
       ${BIN_DIR}/t_test_twitter_utf8 \
       ${BIN_DIR}/t_encode

all: ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS}
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_test_inagist_utf8: t_test_inagist_utf8.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_test_inagist_utf8
	@echo "done!"

${BIN_DIR}/t_test_twitter_utf8: t_test_twitter_utf8.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_test_twitter_utf8
	@echo "done!"

${BIN_DIR}/t_encode: t_encode.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_encode
	@echo "done!"

# DO NOT DELETE THIS LINE
