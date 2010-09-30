PROJECT = test_livemint
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated #-DDEBUG
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/stemmer/ -I../../include/json_parser -I../../src/curl_request_maker -I../../src/keywords_extracter

LINK_DIR = -L../../lib

LIBS = -lstemmer -ljsonparser -lcurlrequestmaker -ltwitcurl -lcurl -lkeywordsextracter

LIB_DIR = ../../lib
BIN_DIR = ../../bin

BINS = ${BIN_DIR}/t_ping_twitter_search_for_livemint \
       ${BIN_DIR}/t_get_followers

all: ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS}
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_ping_twitter_search_for_livemint: t_ping_twitter_search_for_livemint.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_ping_twitter_search_for_livemint
	@echo "done!"

${BIN_DIR}/t_get_followers: t_get_followers.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_get_followers
	@echo "done!"

# DO NOT DELETE THIS LINE
