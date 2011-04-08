include ../Makefile.incl
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../curl_request_maker -I../twitter_api -I../utils -I../keytuples_extracter -I../script_detector -I../ngrams_generator -I../classifier -I../language_detector -I../trends_manager

LINK_DIR = -L../../lib

LIBS = -ltrendsmanager -lkeytuplesextracter -llanguagedetector -lclassifier -lngramsgenerator -lutils -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

BINS = ${BIN_DIR}/t_demo_lang_detect \
       ${BIN_DIR}/x_lang_test_data_generator

all: ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS}
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_demo_lang_detect: t_demo_lang_detect.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_demo_lang_detect
	@echo "done!"

${BIN_DIR}/x_lang_test_data_generator: x_lang_test_data_generator.o
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_lang_test_data_generator
	@echo "done!"

# DO NOT DELETE THIS LINE
