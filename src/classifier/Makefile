include ../../Makefile.incl
PROJECT = classifier
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL} -DCLASSIFIER_DATA_TRAINING_ENABLED -DCLASSIFIER_DATA_TESTING_ENABLED -DCLASS_CONTRIBUTORS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../include/utfcpp -I../utils -I../../extern/curl_request_maker -I../../extern/twitter_api -I../../extern/inagist_api -I../corpus_manager -I../ngrams_generator -I../script_detector

LIBS = -l${PROJECT} -lngramsgenerator -lcorpusmanager -lutils -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_classifier_config \
       ${BIN_DIR}/t_setup_classifier \
       ${BIN_DIR}/t_renumber_classifier_config

OBJS = classifier_config.o naive_bayes_classifier.o classifier.o

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
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_classifier_config: t_classifier_config.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_classifier_config
	@echo "done!"

${BIN_DIR}/t_setup_classifier: t_setup_classifier.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_setup_classifier
	@echo "done!"

${BIN_DIR}/t_renumber_classifier_config: t_renumber_classifier_config.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_renumber_classifier_config
	@echo "done!"

# DO NOT DELETE THIS LINE
