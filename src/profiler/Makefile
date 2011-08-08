include ../../Makefile.incl
PROJECT = profiler
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL} -DNAMED_ENTITIES_ENABLED -DKEYWORDS_ENABLED -DKEYPHRASE_ENABLED -DLANG_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DCLASS_CONTRIBUTORS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../../include/json_parser -I../curl_request_maker -I../twitter_api -I../inagist_api -I../utils -I../script_detector -I../keytuples_extracter -I../corpus_manager -I../ngrams_generator -I../classifier -I../language_detector -I../text_classifier -I../gist_maker

LIBS = -l${PROJECT} -lgistmaker -lngramsgenerator -lcorpusmanager -lutils -lscriptdetector -linagistapi -ltwitterapi -lcurlrequestmaker -ljsonparser -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/x_profiler
#${BIN_DIR}/x_dashboard_populater \
#${BIN_DIR}/t_get_followers

gist = gist_maker.o

OBJS = profiler.o ${gist} #follow_analyser.o

all: ${LIB} ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@echo "done!"

#${LIB}: ${keytuples} ${text} ${lang} ${OBJS}
${LIB}: ${OBJS} ${gist}
	@echo "creating library file ..."
#	@${ARCHIVE} ${LIB} ${keytuples} ${text} ${lang} ${OBJS}
	@${ARCHIVE} ${LIB} ${gist} ${OBJS}
	@ranlib ${LIB}
	@echo "done!"

${gist}:
	@echo "compiling ../gist_maker/gist_maker.cc ..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../gist_maker/gist_maker.cc

#${keytuples}:
#	@echo "compiling ../keytuples_extracter/keytuples_config.cc ..."
#	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_config.cc
#	@echo "compiling ../keytuples_extracter/keytuples_extracter.cc ..."
#	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_extracter.cc
#
#${text}:
#	@echo "compiling ../text_classifier/text_classifier.cc"
#	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../text_classifier/text_classifier.cc
#
#${lang}:
#	@echo "compiling ../language_detector/language_detector.cc"
#	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../language_detector/language_detector.cc

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

# DO NOT DELETE THIS LINE
