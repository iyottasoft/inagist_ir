include ../Makefile.incl
PROJECT = textclassifier
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL} -DTEXT_CLASS_WORDS_ENABLED -DCLASSIFIER_DATA_TRAINING_ENABLED -DCLASSIFIER_DATA_TESTING_ENABLED #-DCLASS_CONTRIBUTORS_ENABLED #-DKEYWORDS_ENABLED -DKEYPHRASE_ENABLED -DHASHTAGS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../include/utfcpp -I../curl_request_maker -I../twitter_api -I../inagist_api -I../script_detector -I../utils -I../keytuples_extracter -I../classifier -I../ngrams_generator

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lngramsgenerator -lutils -lscriptdetector -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_text_classifier \
       ${BIN_DIR}/x_text_classifier_training_data \
       ${BIN_DIR}/t_text_classifier_training_data \
       ${BIN_DIR}/t_text_classifier_test_data

keytuples = keytuples_config.o keytuples_extracter.o
classifier = classifier_config.o corpus_manager.o naive_bayes_classifier.o classifier.o

OBJS = text_classifier.o

all: ${LIB} ${BINS}
	@echo "build done!"

lib: ${LIB}
	@echo "lib done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@echo "done!"

${LIB}: ${keytuples} ${classifier} ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${keytuples} ${classifier} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

${keytuples}:
	@echo "compiling ../keytuples_extracter/keytuples_config.cc ..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_config.cc
	@echo "compiling ../keytuples_extracter/keytuples_extracter.cc ..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_extracter.cc

${classifier}:
	@echo "compiling ../classifier/classifier_config.cc ..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../classifier/classifier_config.cc
	@echo "compiling ../classifier/corpus_manager.cc ..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../classifier/corpus_manager.cc
	@echo "compiling ../classifier/naive_bayes_classifier.cc ..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../classifier/naive_bayes_classifier.cc
	@echo "compiling ../classifier/classifier.cc ..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../classifier/classifier.cc

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
