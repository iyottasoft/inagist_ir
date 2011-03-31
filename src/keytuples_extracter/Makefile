include ../Makefile.incl
PROJECT = keytuplesextracter
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -DDEBUG=${DEBUG_LEVEL} -DKEYPHRASE_ENABLED -DHASHTAGS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../curl_request_maker -I../inagist_api -I../twitter_api -I../utils -I../script_detector -I../ngrams_generator -I../classifier

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lclassifier -lngramsgenerator -lscriptdetector -lutils -ltwitterapi -linagistapi -lcurlrequestmaker -ljsonparser -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin


LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_keytuples_extracter \
       ${BIN_DIR}/t_keywords_manager \
       ${BIN_DIR}/x_keywords_extracter

OBJS = keytuples_extracter.o keywords_manager.o keytuples_config.o

all: ${LIB} ${BINS}
	@echo "build done!"

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
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_keytuples_extracter: t_keytuples_extracter.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keytuples_extracter
	@echo "done!"

${BIN_DIR}/x_keywords_extracter: x_keywords_extracter.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_keywords_extracter
	@echo "done!"

${BIN_DIR}/t_keywords_manager: t_keywords_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keywords_manager
	@echo "done!"

${BIN_DIR}/t_keytuples_from_twitter: t_keytuples_from_twitter.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keytuples_from_twitter
	@echo "done!"

${BIN_DIR}/t_keytuples_from_inagist: t_keytuples_from_inagist.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keytuples_from_inagist
	@echo "done!"

# DO NOT DELETE THIS LINE
