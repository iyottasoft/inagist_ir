include ../../Makefile.incl
PROJECT = profiler
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL} -DI18N_ENABLED -DSCRIPT_DETECTION_ENABLED -DPROFANITY_CHECK_ENABLED -DKEYPHRASE_ENABLED -DNAMED_ENTITIES_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DLANG_ENABLED -DRECSYS_ENABLED #-DLOCATION_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../../include/json_parser -I../../extern/curl_request_maker -I../../extern/twitter_api -I../../extern/inagist_api -I../utils -I../script_detector -I../gist_maker -I../corpus_manager -I../ngrams_generator
#INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../../include/json_parser -I../../extern/curl_request_maker -I../../extern/twitter_api -I../../extern/inagist_api -I../utils -I../script_detector -I../keytuples_extracter -I../corpus_manager -I../ngrams_generator -I../classifier -I../language_classifier -I../text_classifier -I../gist_collector

LIBS = -l${PROJECT} -lcorpusmanager -lngramsgenerator -lutils -lscriptdetector -linagistapi -ltwitterapi -lcurlrequestmaker -ljsonparser -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/x_profiler \
       ${BIN_DIR}/t_profiler
#${BIN_DIR}/x_dashboard_populater \
#${BIN_DIR}/t_get_followers

gist_maker = gist_maker_config.o gist_maker.o
gist = gist_collector.o
keytuples = keytuples_config.o keytuples_extracter.o
classifier = classifier_config.o naive_bayes_classifier.o classifier.o
text = text_classifier.o
lang = language_classifier.o

OBJS = profiler.o #follow_analyser.o

all: ${LIB} ${BINS}
	@echo "build done!"

lib: ${LIB}
	@echo "build lib done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@echo "done!"

${LIB}: ${OBJS} ${gist_maker}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${gist_maker} ${OBJS}
	@ranlib ${LIB}
	@echo "done!"

#${LIB}: ${keytuples} ${classifier} ${text} ${lang} ${gist} ${OBJS}
#	@echo "creating library file ..."
#	@${ARCHIVE} ${LIB} ${keytuples} ${classifier} ${text} ${lang} ${gist} ${OBJS}
#	@ranlib ${LIB}
#	@echo "done!"

${gist_maker}:
	@echo "compiling ../gist_maker/gist_maker_config.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../gist_maker/gist_maker_config.cc
	@echo "compiling ../gist_maker/gist_maker.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../gist_maker/gist_maker.cc

${gist}:
	@echo "compiling ../gist_collector/gist_collector.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../gist_collector/gist_collector.cc

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
	@echo "compiling ../language_classifier/language_classifier.cc"
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../language_classifier/language_classifier.cc

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

#${BIN_DIR}/x_dashboard_populater: x_dashboard_populater.o ${LIB}
#	@echo "creating binary:" $@
#	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_dashboard_populater
#	@echo "done!"

#${BIN_DIR}/t_get_followers: t_get_followers.o ${LIB}
#	@echo "creating binary:" $@
#	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_get_followers
#	@echo "done!"

${BIN_DIR}/x_profiler: x_profiler.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_profiler
	@echo "done!"

${BIN_DIR}/t_profiler: t_profiler.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_profiler
	@echo "done!"

# DO NOT DELETE THIS LINE
