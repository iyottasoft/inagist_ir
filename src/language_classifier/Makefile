include ../../Makefile.incl
PROJECT = languageclassifier
PROJECTFLAGS = -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL} -DLANG_ENABLED -DCLASSIFIER_DATA_TRAINING_ENABLED -DCLASSIFIER_DATA_TESTING_ENABLED #-DCLASS_CONTRIBUTORS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../include/utfcpp -I../utils -I../inagist_api -I../../extern/curl_request_maker -I../../extern/twitter_api -I../../extern/inagist_api -I../script_detector -I../keytuples_extracter -I../corpus_manager -I../ngrams_generator -I../classifier

LIBS = -l${PROJECT} -lngramsgenerator -lcorpusmanager -lscriptdetector -lutils -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_language_classifier \
       ${BIN_DIR}/t_get_lang_corpus \
       ${BIN_DIR}/x_language_classifier_training_data \
       ${BIN_DIR}/t_language_classifier_test_data# \
       #${BIN_DIR}/t_clean_lang_corpus

keytuples = keytuples_config.o keytuples_extracter.o
classifier = classifier_config.o naive_bayes_classifier.o classifier.o

OBJS = language_classifier.o

all: ${LIB} ${BINS}
	@echo "build done!"

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
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_config.cc
	@echo "compiling ../keytuples_extracter/keytuples_extracter.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_extracter.cc

${classifier}:
	@echo "compiling ../classifier/classifier_config.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../classifier/classifier_config.cc
	@echo "compiling ../classifier/naive_bayes_classifier.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../classifier/naive_bayes_classifier.cc
	@echo "compiling ../classifier/classifier.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../classifier/classifier.cc

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_language_classifier: t_language_classifier.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_language_classifier
	@echo "done!"

${BIN_DIR}/x_language_classifier_training_data: x_language_classifier_training_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_language_classifier_training_data
	@echo "done!"

${BIN_DIR}/t_language_classifier_test_data: t_language_classifier_test_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_language_classifier_test_data
	@echo "done!"

${BIN_DIR}/t_get_lang_corpus: t_get_lang_corpus.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_get_lang_corpus
	@echo "done!"

# DO NOT DELETE THIS LINE
