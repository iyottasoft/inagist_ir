PROJECT = languagedetector
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/json_parser -I../utils -I../curl_request_maker -I../twitter_api -I../script_detector -I../ngrams_generator -I../classifier

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lclassifier -lngramsgenerator -lscriptdetector -lutils -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_language_detector \
       ${BIN_DIR}/x_GenerateLangModel

OBJS = language_detector.o language_detector_erl_interface.o

all: ${LIB} ${BINS} language_detector_erl_nif
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB} ${ERL_LIB}
	@rm langd.beam
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_language_detector: t_language_detector.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_language_detector
	@echo "done!"

${BIN_DIR}/x_GenerateLangModel: x_GenerateLangModel.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_GenerateLangModel
	@echo "done!"

ERL_LIB = ${LIB_DIR}/liblangdetect_erl.so
ERL_INCLUDE_DIR = -I/usr/local/lib/erlang/usr/include/ -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include
ERL_LIBS = -lstdc++ -lei -lerl_interface

language_detector_erl_nif:
	@echo "creating erlang library file ..."
	@gcc -fPIC -shared -o ${ERL_LIB} language_detector_erl_nif.c ${INCLUDE_DIR} -L ${LIB_DIR} ${LIBS} ${ERL_INCLUDE_DIR} ${ERL_LIBS} -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib
	@echo ${ERL_LIB}
	@echo "compiling langd.erl"
	@erlc langd.erl

# DO NOT DELETE THIS LINE
