include ../../Makefile.incl
PROJECT = urlwordsextracter
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL} -DI18N_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../../extern/curl_request_maker -I../../extern/inagist_api -I../../extern/twitter_api -I../utils -I../script_detector

LIBS = -l${PROJECT} -lscriptdetector -lutils -ltwitterapi -linagistapi -lcurlrequestmaker -ljsonparser -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_url_words_extracter

OBJS = url_words_extracter.o

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

${BIN_DIR}/t_url_words_extracter: t_url_words_extracter.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_url_words_extracter
	@echo "done!"

# DO NOT DELETE THIS LINE
