include ../Makefile.incl
PROJECT = stemmer
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/porter_stemmer -I../../include/utfcpp -I../utils -I../curl_request_maker -I../twitter_api -I../script_detector

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lutils -lporterstemmer -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_stemmer

OBJS = stemmer.o stemmer_erl_interface.o

all: ${LIB} ${BINS} stemmer_erl_nif
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@rm stemmer.beam
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_stemmer: t_stemmer.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_stemmer
	@echo "done!"

ERL_LIB = ${LIB_DIR}/libstemmer_erl.so
ERL_INCLUDE_DIR = -I/usr/local/lib/erlang/usr/include/ -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include
ERL_LIBS = -lstdc++ -lei -lerl_interface

stemmer_erl_nif:
	@echo "creating erlang library file ..."
	@gcc -fPIC -shared -o ${ERL_LIB} stemmer_erl_nif.c ${INCLUDE_DIR} -L ${LIB_DIR} ${LIBS} ${ERL_INCLUDE_DIR} ${ERL_LIBS} -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib
	@echo ${ERL_LIB}
	@echo "compiling stemmer.erl"
	@erlc stemmer.erl

# DO NOT DELETE THIS LINE
