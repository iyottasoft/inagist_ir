include ../../Makefile.incl
PROJECT = intentdetector
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL} -DINTENT_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../include/utfcpp -I../../extern/curl_request_maker -I../../extern/twitter_api -I../../extern/inagist_api -I../utils -I../script_detector -I../keytuples_extracter

LIBS = -l${PROJECT} -lkeytuplesextracter -lscriptdetector -lutils -ltwitterapi -linagistapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_intent_finder

OBJS = intent_finder.o

keytuples = keytuples_config.o keytuples_extracter.o

all: ${LIB} ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${keytuples} ${LIB}
	@echo "done!"

${keytuples}:
	@echo "compiling ../keytuples_extracter/keytuples_config.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_config.cc
	@echo "compiling ../keytuples_extracter/keytuples_extracter.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_extracter.cc

${LIB}: ${keytuples} ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS} ${keytuples}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_intent_finder: t_intent_finder.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_intent_finder
	@echo "done!"

# DO NOT DELETE THIS LINE
