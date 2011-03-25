include ../Makefile.incl

PROJECT = erlangnif
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I/usr/include/c++/4.4/ -I../../include/json_parser -I../../include/porter_stemmer -I../curl_request_maker -I../twitter_api/ -I../utils -I../stemmer -I../ngrams_generator -I../classifier -I../language_detector -I../keytuples_extracter -I../trends_manager -I../channel_manager -I.
LIB_DIR = ../../lib

ERL_INCLUDE_DIR = -I/usr/local/lib/erlang/usr/include/ -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include
ERL_LIB_DIR = -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib
ERL_LIBS = -lstdc++ -lei -lerl_interface

LIB = ${LIB_DIR}/lib${PROJECT}.so

LIBS = ${LIB_DIR}/libtrends_erl.so ${LIB_DIR}/liblang_erl.so ${LIB_DIR}/libstem_erl.so ${LIB_DIR}/libchannels_erl.so ${LIB_DIR}/libkeytuples_erl.so ${LIB_DIR}/libir_cpp_erl.so

OBJS = tweets.o stem.o trends.o channels.o keytuples.o lang.o ir_cpp.o

all: ${LIB} stem trends channels keytuples lang ir_cpp
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o *.beam ${LIB} ${LIBS}
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

keytuples:
	@echo "creating erlang library file libkeytuples_erl.so ..."
	@gcc -fPIC -shared -o ../../lib/libkeytuples_erl.so keytuples_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I../script_detector -I../utils -I../keytuples_extracter -I. -L ${LIB_DIR} -lerlangnif -lkeytuplesextracter -lutils -lscriptdetector

	@echo "done!"
	@echo "compiling keytuples.erl"
	@erlc keytuples.erl
	@echo "done!" 

trends:
	@echo "creating erlang library file libtrends_erl.so ..."
	@gcc -fPIC -shared -o ../../lib/libtrends_erl.so trends_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I../trends_manager -I. -L ${LIB_DIR} -lerlangnif -ltrendsmanager

	@echo "done!"
	@echo "compiling trends.erl"
	@erlc trends.erl
	@echo "done!" 

stem:
	@echo "creating erlang library file libstem_erl.so ..."
	@gcc -fPIC -shared -o ../../lib/libstem_erl.so stem_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I../../include/porter_stemmer -I../../include/utfcpp -I../stemmer -I../script_detector -I../utils -I../curl_request_maker -I../twitter_api -I. -L ${LIB_DIR} -lerlangnif -lstemmer -ltwitterapi -ljsonparser -lcurlrequestmaker -lutils -lscriptdetector -lporterstemmer -lcurl
	@echo "done!" 
	@echo "compiling stem.erl"
	@erlc stem.erl
	@echo "done!" 

lang:
	@echo "creating erlang library file liblang_erl.so ..."
	@gcc -fPIC -shared -o ../../lib/liblang_erl.so lang_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I../../include/json_parser -I../utils -I../curl_request_maker -I../twitter_api -I../script_detector -I../ngrams_generator -I../classifier -I../language_detector -I. -L ${LIB_DIR} -lerlangnif -llanguagedetector -lclassifier -lngramsgenerator -lstemmer -lporterstemmer -lutils -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!" 
	@echo "compiling lang.erl"
	@erlc lang.erl
	@echo "done!" 

channels:
	@echo "creating erlang library file libchannels_erl.so ..."
	@gcc -fPIC -shared -o ../../lib/libchannels_erl.so channels_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I../../include/json_parser -I../utils -I../curl_request_maker -I../twitter_api -I../channel_manager -I. -L ${LIB_DIR} -lerlangnif -lchannelmanager -lutils -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!" 
	@echo "compiling channels.erl"
	@erlc channels.erl
	@echo "done!" 

ir_cpp:
	@echo "creating erlang library file libir_cpp_erl.so ..."
	@gcc -fPIC -shared -o ../../lib/libir_cpp_erl.so ir_cpp_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I../../include/json_parser -I../curl_request_maker -I../utils -I../twitter_api -I../keytuples_extracter -I../script_detector -I../ngrams_generator -I../classifier -I../language_detector -I../trends_manager -I. -L ${LIB_DIR} -lerlangnif -ltrendsmanager -lkeytuplesextracter -llanguagedetector -lngramsgenerator -lclassifier -ltwitterapi -lutils -lscriptdetector -lcurlrequestmaker -ljsonparser -lcurl

	@echo "done!"
	@echo "compiling ir_cpp.erl"
	@erlc ir_cpp.erl
	@echo "done!" 
