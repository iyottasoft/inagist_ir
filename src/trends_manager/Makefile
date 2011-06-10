include ../Makefile.incl
PROJECT = trendsmanager
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL} -DKEYPHRASE_ENABLED -DHASHTAGS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../curl_request_maker -I../twitter_api -I../inagist_api -I../utils -I../keytuples_extracter -I../script_detector

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lutils -lscriptdetector -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_trends_manager

keytuples = keytuples_config.o keytuples_extracter.o

OBJS = trends_manager.o 

all: ${LIB} ${BINS}
	cp trends_generater.py ${BIN_DIR}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@rm ${BIN_DIR}/trends_generater.py
	@echo "done!"

${LIB}: ${keytuples} ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${keytuples} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

${keytuples}:
	@echo "compiling ../keytuples_extracter/keytuples_config.cc ..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_config.cc
	@echo "compiling ../keytuples_extracter/keytuples_extracter.cc ..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_extracter.cc

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_trends_manager: t_trends_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_trends_manager
	@echo "done!"

# DO NOT DELETE THIS LINE
