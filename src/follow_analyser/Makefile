PROJECT = followanalyser
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated #-DDEBUG
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../curl_request_maker -I../twitter_api -I../keywords_extracter -I../script_detector

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lkeywordsextracter -lscriptdetector -ltwitterapi -lcurlrequestmaker -ljsonparser -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/x_dashboard_populater \
       ${BIN_DIR}/t_get_followers

OBJS = follow_analyser.o

all: ${LIB} ${BINS}
	@echo "copying dashboard_generator.py to ${BIN_DIR}"
	@cp dashboard_generator.py ${BIN_DIR}/
	@echo "copying dashboard_trends_generator.py to ${BIN_DIR}"
	@cp dashboard_trends_generator.py ${BIN_DIR}/
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@rm ${BIN_DIR}/dashboard_generator.py
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB}
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/x_dashboard_populater: x_dashboard_populater.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_dashboard_populater
	@echo "done!"

${BIN_DIR}/t_get_followers: t_get_followers.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_get_followers
	@echo "done!"

# DO NOT DELETE THIS LINE
