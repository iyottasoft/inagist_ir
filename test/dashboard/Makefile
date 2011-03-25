PROJECT = dashboard
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated #-DDEBUG
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../src/curl_request_maker -I../../twitter_api -I../../src/script_detector -I../../src/utils -I../../src/classifier -I../../src/ngrams_generator -I../../src/language_detector -I../../src/keytuples_extracter

LINK_DIR = -L../../lib

LIBS = -lkeytuplesextracter -llanguagedetector -lclassifier -lngramsgenerator -lscriptdetector -lutils -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

BINS = ${BIN_DIR}/x_ping_twitter_search \
       ${BIN_DIR}/t_get_followers

all: ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS}
	@rm ${BIN_DIR}/dashboard_generator.py
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<
	@echo "copying dashboard_generator.py to ${BIN_DIR}"
	@cp dashboard_generator.py ${BIN_DIR}/dashboard_generator.py

${BIN_DIR}/x_ping_twitter_search: x_ping_twitter_search.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_ping_twitter_search
	@echo "done!"

${BIN_DIR}/t_get_followers: t_get_followers.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_get_followers
	@echo "done!"

# DO NOT DELETE THIS LINE
