PROJECT = trendsmanager
ERL_PROJECT = trends
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../curl_request_maker -I../twitter_api -I../keywords_extracter -I../script_detector
ERL_INCLUDE_DIR = -I. -I/usr/local/lib/erlang/usr/include -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lkeywordsextracter -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
ERL_LIBS = -l${ERL_PROJECT} -lstdc++ -lei -lerl_interface

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so
ERL_LIB = ${LIB_DIR}/lib${ERL_PROJECT}.so

BINS = ${BIN_DIR}/t_trends_manager \
       ${BIN_DIR}/t_trends_erl_nif

OBJS = trends_manager.o 
ERL_OBJS = trends_erl_nif.o 

all: ${LIB} ${ERL_LIB} ${BINS}
	cp trends_generater.py ${BIN_DIR}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB} ${ERL_LIB}
	@rm ${BIN_DIR}/trends_generater.py
	@rm trends.beam
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

${ERL_LIB}: ${ERL_OBJS}
	@echo "creating erlang library file ..."
	@${ARCHIVE} ${ERL_LIB} ${ERL_OBJS}
	@echo "compiling trends.erl"
	@erlc trends.erl
	@echo "done!"

.c.o:
	@echo "compiling ..." $< "..."
	@gcc -shared -fPIC -c ${INCLUDE_DIR} ${ERL_INCLUDE_DIR} $<

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} ${ERL_INCLUDE_DIR} $<

${BIN_DIR}/t_trends_manager: t_trends_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_trends_manager
	@echo "done!"

${BIN_DIR}/t_trends_erl_nif: t_trends_erl_nif.o ${ERL_LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${ERL_INCLUDE_DIR}${LINK_DIR} ${ERL_LIBS} -o ${BIN_DIR}/t_trends_erl_nif
	@echo "done!"

# DO NOT DELETE THIS LINE
