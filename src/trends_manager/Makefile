include = ../Makefile.incl
PROJECT = trendsmanager
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../curl_request_maker -I../twitter_api -I../utils -I../keywords_extracter -I../script_detector

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lkeywordsextracter -lutils -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_trends_manager \
       ${BIN_DIR}/t_show_unsafe_trends

OBJS = trends_manager.o 

all: ${LIB} ${BINS} trends
	cp trends_generater.py ${BIN_DIR}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@rm ${BIN_DIR}/trends_generater.py
	@rm trends.beam
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_trends_manager: t_trends_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_trends_manager
	@echo "done!"

${BIN_DIR}/t_show_unsafe_trends: t_show_unsafe_trends.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_show_unsafe_trends
	@echo "done!"

ERL_LIB = ${LIB_DIR}/libtrends.so
trends:
	@echo "creating erlang library file ..."
	@gcc -fPIC -shared -o ${ERL_LIB} trends_erl_nif.c -I/usr/local/lib/erlang/usr/include/ -I../../include/json_parser -I../curl_request_maker -I../utils -I../twitter_api -I../keywords_extracter -I../script_detector -I../trends_manager -L../../lib -ltrendsmanager -lkeywordsextracter -lscriptdetector -ltwitterapi -lutils -lcurlrequestmaker -ljsonparser -lcurl -lstdc++ -lei -lerl_interface  -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib
	@echo ${ERL_LIB}
	@echo "compiling trends.erl"
	@erlc trends.erl

# DO NOT DELETE THIS LINE
