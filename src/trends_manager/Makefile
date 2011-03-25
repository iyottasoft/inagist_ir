include ../Makefile.incl
PROJECT = trendsmanager
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../curl_request_maker -I../twitter_api -I../utils -I../keytuples_extracter -I../script_detector -I../ngrams_generator -I../classifier -I../language_detector

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lkeytuplesextracter -llanguagedetector -lclassifier -lngramsgenerator -lutils -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_trends_manager

OBJS = trends_manager.o 

all: ${LIB} ${BINS}
	cp trends_generater.py ${BIN_DIR}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@rm ${BIN_DIR}/trends_generater.py
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_trends_manager: t_trends_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_trends_manager
	@echo "done!"

# DO NOT DELETE THIS LINE
