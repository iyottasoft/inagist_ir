include ../Makefile.incl
PROJECT = textclassifier
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../curl_request_maker -I../twitter_api -I../script_detector -I../utils -I../classifier -I../keytuples_extracter

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lkeytuplesextracter -lclassifier -lutils -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_text_classifier \
       ${BIN_DIR}/x_textclass_training_data \
       ${BIN_DIR}/t_textclass_training_data

OBJS = text_classifier.o

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

${BIN_DIR}/t_text_classifier: t_text_classifier.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_text_classifier
	@echo "done!"

${BIN_DIR}/x_textclass_training_data: x_textclass_training_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_textclass_training_data
	@echo "done!"

${BIN_DIR}/t_textclass_training_data: t_textclass_training_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_textclass_training_data
	@echo "done!"

# DO NOT DELETE THIS LINE
