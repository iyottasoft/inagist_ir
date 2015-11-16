include ../../Makefile.incl
PROJECT = trendsmanager
PROJECTFLAGS = -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL} -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../../extern/curl_request_maker -I../../extern/twitter_api -I../../extern/inagist_api -I../utils -I../keytuples_extracter -I../script_detector

LIBS = -l${PROJECT} -lutils -lscriptdetector -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_trends_manager

keytuples = keytuples_config.o keytuples_extracter.o

OBJS = trends_manager.o 

all: ${LIB} ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@echo "done!"

${LIB}: ${keytuples} ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${keytuples} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

${keytuples}:
	@echo "compiling ../keytuples_extracter/keytuples_config.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_config.cc
	@echo "compiling ../keytuples_extracter/keytuples_extracter.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_extracter.cc

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_trends_manager: t_trends_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_trends_manager
	@echo "done!"

# DO NOT DELETE THIS LINE
