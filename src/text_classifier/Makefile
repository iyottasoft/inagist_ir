include ../../Makefile.incl
PROJECT = textclassifier
PROJECTFLAGS = -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL} -DTEXT_CLASSIFICATION_ENABLED -DCLASSIFIER_DATA_TRAINING_ENABLED -DCLASSIFIER_DATA_TESTING_ENABLED #-DCLASS_CONTRIBUTORS_ENABLED #-DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../include/utfcpp -I../../extern/curl_request_maker -I../../extern/twitter_api -I../../extern/inagist_api -I../script_detector -I../utils -I../keytuples_extracter -I../corpus_manager -I../ngrams_generator -I../classifier

LIBS = -l${PROJECT} -lngramsgenerator -lcorpusmanager -lutils -lscriptdetector -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_text_classifier \
       ${BIN_DIR}/x_text_classifier_training_data \
       ${BIN_DIR}/t_text_classifier_training_data \
       ${BIN_DIR}/x_text_classifier_testing_data

keytuples = keytuples_config.o keytuples_extracter.o
classifier = classifier_config.o naive_bayes_classifier.o classifier.o

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

${BIN_DIR}/t_text_classifier: t_text_classifier.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_text_classifier
	@echo "done!"

${BIN_DIR}/x_text_classifier_training_data: x_text_classifier_training_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_text_classifier_training_data
	@echo "done!"

${BIN_DIR}/t_text_classifier_training_data: t_text_classifier_training_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_text_classifier_training_data
	@echo "done!"

${BIN_DIR}/x_text_classifier_testing_data: x_text_classifier_testing_data.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_text_classifier_testing_data
	@echo "done!"

#${BIN_DIR}/t_clean_text_corpus: t_clean_text_corpus.o ${LIB}
#	@echo "creating binary:" $@
#	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_clean_text_corpus
#	@echo "done!"

#${BIN_DIR}/t_text_classifier_training_data_normalizer: t_text_classifier_training_data_normalizer.o ${LIB}
#	@echo "creating binary:" $@
#	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_text_classifier_training_data_normalizer
#	@echo "done!"

#${BIN_DIR}/t_make_classifier_dictionary: t_make_classifier_dictionary.o ${LIB}
#	@echo "creating binary:" $@
#	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_make_classifier_dictionary
#	@echo "done!"

# DO NOT DELETE THIS LINE
