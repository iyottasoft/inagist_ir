include ../Makefile.incl
PROJECT = keywordsextracter
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -DDEBUG=${DEBUG_LEVEL} -DKEYPHRASE_ENABLED -DHASHTAGS_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../curl_request_maker -I../inagist_api -I../twitter_api -I../utils -I../script_detector -I../ngrams_generator -I../classifier -I../language_detector

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -llanguagedetector -lclassifier -lngramsgenerator -lscriptdetector -lutils -ltwitterapi -linagistapi -lcurlrequestmaker -ljsonparser -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin


LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_keywords_extract \
       ${BIN_DIR}/t_keywords_manager \
       ${BIN_DIR}/t_keywords_from_twitter \
       ${BIN_DIR}/t_keywords_from_inagist \
       ${BIN_DIR}/x_keywords_extract

OBJS = keywords_extract.o keywords_manager.o

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
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_keywords_extract: t_keywords_extract.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keywords_extract
	@echo "done!"

${BIN_DIR}/x_keywords_extract: x_keywords_extract.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_keywords_extract
	@echo "done!"

${BIN_DIR}/t_keywords_manager: t_keywords_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keywords_manager
	@echo "done!"

${BIN_DIR}/t_keywords_from_twitter: t_keywords_from_twitter.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keywords_from_twitter
	@echo "done!"

${BIN_DIR}/t_keywords_from_inagist: t_keywords_from_inagist.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keywords_from_inagist
	@echo "done!"

${BIN_DIR}/t_keywords_from_stream: t_keywords_from_stream.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keywords_from_stream
	@echo "done!"

# DO NOT DELETE THIS LINE
