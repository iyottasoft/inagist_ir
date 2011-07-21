include ../../Makefile.incl
PROJECT = gistmaker
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL} -DKEYWORDS_ENABLED -DKEYPHRASE_ENABLED -DHASHTAGS_ENABLED -DLANG_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -DCLASS_CONTRIBUTORS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../curl_request_maker -I../twitter_api -I../inagist_api -I../utils -I../keytuples_extracter -I../script_detector -I../ngrams_generator -I../classifier -I../language_detector -I../text_classifier

LIBS = -l${PROJECT} -lngramsgenerator -lutils -lscriptdetector -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_gist_maker

keytuples = keytuples_config.o keytuples_extracter.o
classifier = classifier_config.o corpus_manager.o naive_bayes_classifier.o classifier.o
text = text_classifier.o
lang = language_detector.o

OBJS = gist_maker.o 

all: ${LIB} ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@echo "done!"

${LIB}: ${keytuples} ${classifier} ${text} ${lang} ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${keytuples} ${classifier} ${text} ${lang} ${OBJS}
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

${text}:
	@echo "compiling ../text_classifier/text_classifier.cc"
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../text_classifier/text_classifier.cc

${lang}:
	@echo "compiling ../language_detector/language_detector.cc"
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../language_detector/language_detector.cc

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_gist_maker: t_gist_maker.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_gist_maker
	@echo "done!"

# DO NOT DELETE THIS LINE
