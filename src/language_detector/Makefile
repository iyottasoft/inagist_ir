include ../Makefile.incl
PROJECT = languagedetector
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL} -DKEYWORDS_DISABLED -DLANG_WORDS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../include/utfcpp -I../utils -I../inagist_api -I../curl_request_maker -I../twitter_api -I../script_detector -I../keytuples_extracter -I../ngrams_generator -I../classifier

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lclassifier -lngramsgenerator -lscriptdetector -lutils -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_language_detector \
       ${BIN_DIR}/x_language_detector_training_data \
       ${BIN_DIR}/t_language_detector_test_data \
       ${BIN_DIR}/t_get_lang_corpus

keytuples = keytuples_config.o keytuples_extracter.o

OBJS = language_detector.o

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
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_config.cc
	@echo "compiling ../keytuples_extracter/keytuples_extracter.cc ..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_extracter.cc

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_language_detector: t_language_detector.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_language_detector
	@echo "done!"

${BIN_DIR}/x_language_detector_training_data: x_language_detector_training_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_language_detector_training_data
	@echo "done!"

${BIN_DIR}/t_language_detector_test_data: t_language_detector_test_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_language_detector_test_data
	@echo "done!"

${BIN_DIR}/t_get_lang_corpus: t_get_lang_corpus.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_get_lang_corpus
	@echo "done!"

# DO NOT DELETE THIS LINE
