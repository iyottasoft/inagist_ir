include ../Makefile.incl
PROJECT = profiler
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -DDEBUG=${DEBUG_LEVEL} -DKEYWORDS_ENABLED -DHASHTAGS_ENABLED -DKEYPHRASE_ENABLED -DLANG_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DCLASS_CONTRIBUTORS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../../include/json_parser -I../curl_request_maker -I../twitter_api -I../inagist_api -I../utils -I../script_detector -I../keytuples_extracter -I../ngrams_generator -I../classifier -I../language_detector -I../text_classifier -I../gist_maker

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lgistmaker -lngramsgenerator -lutils -lscriptdetector -linagistapi -ltwitterapi -lcurlrequestmaker -ljsonparser -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/x_profiler
#${BIN_DIR}/x_dashboard_populater \
#${BIN_DIR}/t_get_followers

gist = gist_maker.o

OBJS = profiler.o ${gist} #follow_analyser.o

all: ${LIB} ${BINS}
	@echo "copying dashboard_generator.py to ${BIN_DIR}"
	@cp dashboard_generator.py ${BIN_DIR}/
	@echo "copying dashboard_trends_generator.py to ${BIN_DIR}"
	@cp dashboard_trends_generator.py ${BIN_DIR}/
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@rm ${BIN_DIR}/dashboard_generator.py
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
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../gist_maker/gist_maker.cc

#${keytuples}:
#	@echo "compiling ../keytuples_extracter/keytuples_config.cc ..."
#	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_config.cc
#	@echo "compiling ../keytuples_extracter/keytuples_extracter.cc ..."
#	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../keytuples_extracter/keytuples_extracter.cc
#
#${text}:
#	@echo "compiling ../text_classifier/text_classifier.cc"
#	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../text_classifier/text_classifier.cc
#
#${lang}:
#	@echo "compiling ../language_detector/language_detector.cc"
#	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ../language_detector/language_detector.cc

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

#${BIN_DIR}/x_dashboard_populater: x_dashboard_populater.o ${LIB}
#	@echo "creating binary:" $@
#	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_dashboard_populater
#	@echo "done!"

#${BIN_DIR}/t_get_followers: t_get_followers.o ${LIB}
#	@echo "creating binary:" $@
#	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_get_followers
#	@echo "done!"

${BIN_DIR}/x_profiler: x_profiler.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_profiler
	@echo "done!"

# DO NOT DELETE THIS LINE
