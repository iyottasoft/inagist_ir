include ../../Makefile.incl
PROJECT = gistmaker
PROJECTFLAGS = -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL} -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED #-DLANG_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DCLASS_CONTRIBUTORS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../curl_request_maker -I../twitter_api -I../inagist_api -I../utils -I../keytuples_extracter -I../script_detector -I../corpus_manager -I../ngrams_generator -I../classifier -I../language_detector -I../text_classifier

LIBS = -l${PROJECT} -lngramsgenerator -lcorpusmanager -lutils -lscriptdetector -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_gist_maker

keytuples = keytuples_config.o keytuples_extracter.o
classifier = classifier_config.o naive_bayes_classifier.o classifier.o
text = text_classifier.o
lang = language_detector.o

OBJS = gist_maker.o 

all: ${LIB} ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@echo "done!"

#${LIB}: ${keytuples} ${classifier} ${text} ${lang} ${OBJS}
${LIB}: ${keytuples} ${OBJS}
	@echo "creating library file ..."
#@${ARCHIVE} ${LIB} ${keytuples} ${classifier} ${text} ${lang} ${OBJS}
	@${ARCHIVE} ${LIB} ${keytuples} ${OBJS}
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

${text}:
	@echo "compiling ../text_classifier/text_classifier.cc"
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../text_classifier/text_classifier.cc

${lang}:
	@echo "compiling ../language_detector/language_detector.cc"
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../language_detector/language_detector.cc

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_gist_maker: t_gist_maker.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_gist_maker
	@echo "done!"

${BIN_DIR}/t_get_intent: t_get_intent.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_get_intent
	@echo "done!"

# DO NOT DELETE THIS LINE
