include ../../Makefile.incl
PROJECT = test_utf8
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -DDEBUG -D_CPLUSPLUS
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../src/curl_request_maker -I../../include/utfcpp -I../../src/twitter_api -I../../src/script_detector

LIBS = -l${PROJECT} -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_encode \
       ${BIN_DIR}/t_test_utfcpp

OBJS = test_utfcpp.o

all: ${LIB} ${BINS} test_utfcpp_erl_nif
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS}
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_encode: t_encode.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_encode
	@echo "done!"

${BIN_DIR}/t_test_utfcpp: t_test_utfcpp.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_test_utfcpp
	@echo "done!"

ERL_LIB = ${LIB_DIR}/libtestutfcpp_erl.so
ERL_INCLUDE_DIR = -I/usr/local/lib/erlang/usr/include/ -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include
ERL_LIBS = -lstdc++ -lei -lerl_interface

test_utfcpp_erl_nif:
	@echo "creating erlang library file ..."
	@gcc -fPIC -shared -o ${ERL_LIB} test_utfcpp_erl_nif.c ${INCLUDE_DIR} -L ${LIB_DIR} ${LIBS} ${ERL_INCLUDE_DIR} ${ERL_LIBS} -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib
	@echo ${ERL_LIB}
	@echo "compiling test_utfcpp.erl"
	@erlc test_utfcpp.erl

# DO NOT DELETE THIS LINE
