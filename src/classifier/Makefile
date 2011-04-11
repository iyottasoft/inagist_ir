include ../Makefile.incl
PROJECT = classifier
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../utils -I../curl_request_maker -I../twitter_api -I../ngrams_generator

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lngramsgenerator -lutils -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_corpus_manager \
       ${BIN_DIR}/t_config_reader \
       ${BIN_DIR}/t_setup_classifier

OBJS = config_reader.o corpus_manager.o naive_bayes_classifier.o classifier.o

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

${BIN_DIR}/t_corpus_manager: t_corpus_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_corpus_manager
	@echo "done!"

${BIN_DIR}/t_config_reader: t_config_reader.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_config_reader
	@echo "done!"

${BIN_DIR}/t_setup_classifier: t_setup_classifier.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_setup_classifier
	@echo "done!"

# DO NOT DELETE THIS LINE
