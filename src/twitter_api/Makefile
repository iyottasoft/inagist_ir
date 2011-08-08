include ../../Makefile.incl
PROJECT = twitterapi
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../src/curl_request_maker

LIBS = -l${PROJECT} -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_twitter_searcher \
       ${BIN_DIR}/t_twitter_api

OBJS = twitter_searcher.o twitter_api.o

all: ${LIB} ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB}
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_twitter_searcher: t_twitter_searcher.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_twitter_searcher
	@echo "done!"

${BIN_DIR}/t_twitter_api: t_twitter_api.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_twitter_api
	@echo "done!"

# DO NOT DELETE THIS LINE
