include ../../Makefile.incl
PROJECT = corpusmanager
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ # -I../../include/json_parser -I../utils -I../curl_request_maker -I../twitter_api -I../inagist_api -I../ngrams_generator

LIBS = -l${PROJECT} # -lngramsgenerator -lutils -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_corpus_manager

OBJS = corpus_manager.o

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

${BIN_DIR}/t_corpus_manager: t_corpus_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_corpus_manager
	@echo "done!"

# DO NOT DELETE THIS LINE
