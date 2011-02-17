include ../Makefile.incl
PROJECT = channelmanager
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../utils -I../curl_request_maker -I../twitter_api -I../script_detector -I../ngrams_generator -I../classifier -I../language_detector -I../keywords_extracter

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lutils -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_channel_manager \
       ${BIN_DIR}/x_generate_channel_model \
       ${BIN_DIR}/t_generate_channel_model

OBJS = channel_manager.o channel_manager_erl_interface.o

all: ${LIB} ${BINS} channel_manager_erl_nif
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB} ${ERL_LIB}
	@rm channels.beam
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_channel_manager: t_channel_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_channel_manager
	@echo "done!"

${BIN_DIR}/x_generate_channel_model: x_generate_channel_model.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_generate_channel_model
	@echo "done!"

${BIN_DIR}/t_generate_channel_model: t_generate_channel_model.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} -lkeywordsextracter -llanguagedetector -lscriptdetector -lclassifier -lngramsgenerator ${LIBS} -o ${BIN_DIR}/t_generate_channel_model
	@echo "done!"

ERL_LIB = ${LIB_DIR}/libchannelmanager_erl.so
ERL_INCLUDE_DIR = -I/usr/local/lib/erlang/usr/include/ -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include
ERL_LIBS = -lstdc++ -lei -lerl_interface

channel_manager_erl_nif:
	@echo "creating erlang library file ..."
	@gcc -fPIC -shared -o ${ERL_LIB} channel_manager_erl_nif.c ${INCLUDE_DIR} -L ${LIB_DIR} ${LIBS} ${ERL_INCLUDE_DIR} ${ERL_LIBS} -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib
	@echo ${ERL_LIB}
	@echo "compiling channels.erl"
	@erlc channels.erl

# DO NOT DELETE THIS LINE
