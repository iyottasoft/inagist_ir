include ../Makefile.incl
PROJECT = textclassifier
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL} -DKEYPHRASE_ENABLED -DHASHTAGS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../include/utfcpp -I../curl_request_maker -I../twitter_api -I../inagist_api -I../script_detector -I../utils -I../keytuples_extracter -I../classifier -I../ngrams_generator -I../language_detector

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -llanguagedetector -lngramsgenerator -lclassifier -lutils -lscriptdetector -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_text_classifier \
       ${BIN_DIR}/x_text_classifier_training_data \
       ${BIN_DIR}/t_text_classifier_training_data \
       ${BIN_DIR}/t_text_classifier_test_data

keytuples = keytuples_config.o keytuples_extracter.o

OBJS = text_classifier.o

all: ${LIB} ${BINS}
	@echo "build done!"

lib: ${LIB}
	@echo "lib done!"

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
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_config.cc
	@echo "compiling ../keytuples_extracter/keytuples_extracter.cc ..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_extracter.cc

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_text_classifier: t_text_classifier.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_text_classifier
	@echo "done!"

${BIN_DIR}/x_text_classifier_training_data: x_text_classifier_training_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_text_classifier_training_data
	@echo "done!"

${BIN_DIR}/t_text_classifier_training_data: t_text_classifier_training_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_text_classifier_training_data
	@echo "done!"

${BIN_DIR}/t_text_classifier_test_data: t_text_classifier_test_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_text_classifier_test_data
	@echo "done!"

# DO NOT DELETE THIS LINE
