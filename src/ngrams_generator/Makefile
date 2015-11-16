include ../../Makefile.incl
PROJECT = ngramsgenerator
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../include/utfcpp -I../utils -I../../extern/curl_request_maker -I../../extern/twitter_api -I../script_detector -I../corpus_manager

LIBS = -l${PROJECT} -lscriptdetector -lutils -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_ngrams_generator

OBJS = ngrams_generator.o

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

${BIN_DIR}/t_ngrams_generator: t_ngrams_generator.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_ngrams_generator
	@echo "done!"

# DO NOT DELETE THIS LINE
