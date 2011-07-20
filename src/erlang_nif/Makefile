include ../Makefile.incl

PROJECT = erlangnif
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

LIB_DIR = ../../lib

ERL_INCLUDE_DIR = -I/usr/local/lib/erlang/usr/include/ -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include
ERL_LIB_DIR = -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib
ERL_LIBS = -lstdc++ -lei -lerl_interface

LIBS = ${LIB_DIR}/libtweets.a ${LIB_DIR}/libstem_nif.a ${LIB_DIR}/libstem_erl.so ${LIB_DIR}/libkeytuples_nif.a ${LIB_DIR}/libkeytuples_erl.so ${LIB_DIR}/libtrends_nif.a ${LIB_DIR}/libtrends_erl.so ${LIB_DIR}/liblang_nif.a ${LIB_DIR}/liblang_erl.so ${LIB_DIR}/libtext_class_nif.a ${LIB_DIR}/libtext_class_erl.so ${LIB_DIR}/libgist_nif.a ${LIB_DIR}/libgist_erl.so ${LIB_DIR}/libchannels_nif.a ${LIB_DIR}/libchannels_erl.so ${LIB_DIR}/libprofile_nif.a ${LIB_DIR}/libprofile_nif.so

all: tweets stem keywords keytuples intent trends lang text_class gist channels profile
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o *.beam ${LIBS}
	@echo "done!"

tweets:
	@echo "creating library ${LIB_DIR}/libtweets.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -c tweets.cc -I../twitter_api
	@ar -r ${LIB_DIR}/libtweets.a tweets.o
	@ranlib ${LIB_DIR}/libtweets.a
	@echo "done!"

stem:
	@echo "creating library ${LIB_DIR}/libstem_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -c stem.cc -I../../include/porter_stemmer -I../utils -I../stemmer
	@ar -r ${LIB_DIR}/libstem_nif.a stem.o
	@ranlib ${LIB_DIR}/libstem_nif.a
	@echo "done!"
	@echo "creating erlang library file ${LIB_DIR}/libstem_erl.so ..."
	@gcc -fPIC -shared -o ${LIB_DIR}/libstem_erl.so stem_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I../../include/json_parser -I../../include/porter_stemmer -I../../include/utfcpp -I../stemmer -I../script_detector -I../utils -I../curl_request_maker -I../twitter_api -I. -L${LIB_DIR} -lstem_nif -lstemmer -lporterstemmer -ltweets -ltwitterapi -lutils -lscriptdetector -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling stem.erl"
	@erlc stem.erl
	@echo "done!"

keywords:
	@echo "creating library ${LIB_DIR}/libkeywords_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -DKEYWORDS_ENABLED -c ../keytuples_extracter/keytuples_config.cc ../keytuples_extracter/keytuples_extracter.cc keywords.cc -I../../include/utfcpp -I../utils -I../script_detector -I../keytuples_extracter
	@ar -r ${LIB_DIR}/libkeywords_nif.a keytuples_config.o keytuples_extracter.o keywords.o
	@ranlib ${LIB_DIR}/libkeywords_nif.a
	@rm keytuples_config.o keytuples_extracter.o
	@echo "done!"
	@echo "creating erlang library file ${LIB_DIR}/libkeywords_erl.so ..."
	@gcc -fPIC -shared -o ${LIB_DIR}/libkeywords_erl.so keywords_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LIB_DIR} -lkeywords_nif -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling keywords.erl"
	@erlc keywords.erl
	@echo "done!"

keytuples:
	@echo "creating library ${LIB_DIR}/libkeytuples_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -DKEYWORDS_ENABLED -DKEYPHRASE_ENABLED -DHASHTAGS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -c ../keytuples_extracter/keytuples_config.cc ../keytuples_extracter/keytuples_extracter.cc keytuples.cc -I../../include/utfcpp -I../utils -I../script_detector -I../keytuples_extracter
	@ar -r ${LIB_DIR}/libkeytuples_nif.a keytuples_config.o keytuples_extracter.o keytuples.o
	@rm keytuples_config.o keytuples_extracter.o
	@ranlib ${LIB_DIR}/libkeytuples_nif.a
	@echo "done!"
	@echo "creating erlang library file ${LIB_DIR}/libkeytuples_erl.so ..."
	@gcc -fPIC -shared -o ${LIB_DIR}/libkeytuples_erl.so keytuples_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LIB_DIR} -lkeytuples_nif -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling keytuples.erl"
	@erlc keytuples.erl
	@echo "done!"

intent:
	@echo "creating library ${LIB_DIR}/libintent_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -DINTENT_ENABLED -c ../keytuples_extracter/keytuples_config.cc ../keytuples_extracter/keytuples_extracter.cc intent.cc -I../../include/utfcpp -I../utils -I../script_detector -I../keytuples_extracter
	@ar -r ${LIB_DIR}/libintent_nif.a keytuples_config.o keytuples_extracter.o intent.o
	@ranlib ${LIB_DIR}/libintent_nif.a
	@rm keytuples_config.o keytuples_extracter.o
	@echo "done!"
	@echo "creating erlang library file ${LIB_DIR}/libintent_erl.so ..."
	@gcc -fPIC -shared -o ${LIB_DIR}/libintent_erl.so intent_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LIB_DIR} -lintent_nif -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling intent.erl"
	@erlc intent.erl
	@echo "done!"

sentiment:
	@echo "creating library ${LIB_DIR}/libsentiment_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -DSENTIMENT_ENABLED -c ../keytuples_extracter/keytuples_config.cc ../keytuples_extracter/keytuples_extracter.cc sentiment.cc -I../../include/utfcpp -I../utils -I../script_detector -I../keytuples_extracter
	@ar -r ${LIB_DIR}/libsentiment_nif.a keytuples_config.o keytuples_extracter.o sentiment.o
	@ranlib ${LIB_DIR}/libsentiment_nif.a
	@rm keytuples_config.o keytuples_extracter.o
	@echo "done!"
	@echo "creating erlang library file ${LIB_DIR}/libsentiment_erl.so ..."
	@gcc -fPIC -shared -o ${LIB_DIR}/libsentiment_erl.so sentiment_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LIB_DIR} -lsentiment_nif -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling sentiment.erl"
	@erlc sentiment.erl
	@echo "done!"

trends:
	@echo "creating library ${LIB_DIR}/libtrends_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -c trends.cc -I../utils -I../keytuples_extracter -I../trends_manager
	@ar -r ${LIB_DIR}/libtrends_nif.a trends.o
	@ranlib ${LIB_DIR}/libtrends_nif.a
	@echo "done!"
	@echo "creating erlang library file ${LIB_DIR}/libtrends_erl.so ..."
	@gcc -fPIC -shared -o ${LIB_DIR}/libtrends_erl.so trends_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L ${LIB_DIR} -ltrends_nif -ltrendsmanager -lkeytuplesextracter -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling trends.erl"
	@erlc trends.erl
	@echo "done!"

lang:
	@echo "creating library ${LIB_DIR}/liblang_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -c -DLANG_WORDS_ENABLED ../keytuples_extracter/keytuples_config.cc ../keytuples_extracter/keytuples_extracter.cc ../language_detector/language_detector.cc lang.cc -I../../include/utfcpp -I../script_detector -I../utils -I../keytuples_extracter -I../ngrams_generator -I../classifier -I../language_detector
	@ar -r ${LIB_DIR}/liblang_nif.a keytuples_config.o keytuples_extracter.o language_detector.o lang.o
	@ranlib ${LIB_DIR}/liblang_nif.a
	@echo "done!"
	@echo "creating erlang library file ${LIB_DIR}/liblang_erl.so ..."
	@gcc -fPIC -shared -o ${LIB_DIR}/liblang_erl.so lang_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LIB_DIR} -llang_nif -lclassifier -lngramsgenerator -lutils -lscriptdetector -ltweets -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling lang.erl"
	@erlc lang.erl
	@echo "done!"

text_class:
	@echo "creating library ${LIB_DIR}/libtext_class_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -c -DTEXT_CLASS_WORDS_ENABLED ../keytuples_extracter/keytuples_config.cc ../keytuples_extracter/keytuples_extracter.cc ../text_classifier/text_classifier.cc text_class.cc -I../../include/utfcpp -I../script_detector -I../utils -I../keytuples_extracter -I../ngrams_generator -I../classifier -I../text_classifier
	@ar -r ${LIB_DIR}/libtext_class_nif.a keytuples_config.o keytuples_extracter.o text_classifier.o text_class.o
	@ranlib ${LIB_DIR}/libtext_class_nif.a
	@echo "done!"
	@echo "creating erlang library file ${LIB_DIR}/libtext_class_erl.so ..."
	@gcc -fPIC -shared -o ../../lib/libtext_class_erl.so text_class_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L ${LIB_DIR} -ltext_class_nif -lclassifier -lutils -lscriptdetector -ltweets -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling text_class.erl"
	@erlc text_class.erl
	@echo "done!"

channels:
	@echo "creating library ${LIB_DIR}/libchannels_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -c channels.cc -I../channel_manager
	@ar -r ${LIB_DIR}/libchannels_nif.a channels.o
	@ranlib ${LIB_DIR}/libchannels_nif.a
	@echo "done!"
	@echo "creating erlang library file libchannels_erl.so ..."
	@gcc -fPIC -shared -o ${LIB_DIR}/libchannels_erl.so channels_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L ${LIB_DIR} -lchannels_nif -lchannelmanager -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling channels.erl"
	@erlc channels.erl
	@echo "done!"

KEYTUPLES = -I../keytuples_extracter ../keytuples_extracter/keytuples_config.cc ../keytuples_extracter/keytuples_extracter.cc \
            -DKEYWORDS_ENABLED \
            -DINTENT_ENABLED \
            -DSENTIMENT_ENABLED \
            -DKEYPHRASE_ENABLED \
            -DHASHTAGS_ENABLED

KEYTUPLES_OBJS = keytuples_config.o keytuples_extracter.o

CLASSIFIER =  -I../ngrams_generator -I../classifier ../classifier/classifier_config.cc ../classifier/corpus_manager.cc ../classifier/naive_bayes_classifier.cc ../classifier/classifier.cc
CLASSIFIER_OBJS = classifier_config.o corpus_manager.o naive_bayes_classifier.o classifier.o

LANG =  -I../language_detector -DLANG_ENABLED ../language_detector/language_detector.cc
LANG_OBJS = language_detector.o

TEXT_CLASSIFICATION = -DTEXT_CLASSIFICATION_ENABLED -I../text_classifier ../text_classifier/text_classifier.cc 
TEXT_CLASSIFICATION_OBJS = text_classifier.o

GIST_COMPONENTS =  -I../gist_maker ../gist_maker/gist_maker.cc gist.cc \
                  ${KEYTUPLES} \
                  ${CLASSIFIER} \
                  ${TEXT_CLASSIFICATION} \
                  ${LANG}

GIST_OBJS = gist_maker.o gist.o \
            ${KEYTUPLES_OBJS} \
            ${CLASSIFIER_OBJS} \
            ${TEXT_CLASSIFICATION_OBJS} \
            ${LANG_OBJS}

gist:
	@echo "creating library ${LIB_DIR}/libgist_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -c ${GIST_COMPONENTS} -I../../include/utfcpp -I../twitter_api -I../inagist_api -I../script_detector -I../utils
	@ar -r ${LIB_DIR}/libgist_nif.a ${GIST_OBJS}
	@ranlib ${LIB_DIR}/libgist_nif.a
	@echo "done!"
	@echo "creating erlang library file libgist_erl.so ..."
	@gcc -fPIC -shared -o ../../lib/libgist_erl.so gist_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L ${LIB_DIR} -lgist_nif -lngramsgenerator -ltweets -ltwitterapi -linagistapi -lutils -lscriptdetector -lcurlrequestmaker -ljsonparser -lcurl
	@echo "done!"
	@echo "compiling gist.erl"
	@erlc gist.erl
	@echo "done!"

test_gist:
	gcc -g t_test_gist.c -o ../../bin/t_test_gist -I. -lstdc++ -L ${LIB_DIR} -lgist_nif -lngramsgenerator -ltweets -ltwitterapi -linagistapi -lutils -lscriptdetector -lcurlrequestmaker -ljsonparser -lcurl
	@echo "done!"

profile:
	@echo "creating library ${LIB_DIR}/libprofile_nif.a ..."
	@g++ ${CXXFLAGS} -fPIC -shared -DKEYWORDS_ENABLED -DHASHTAGS_ENABLED -DKEYPHRASE_ENABLED -DLANG_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DCLASS_CONTRIBUTORS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -I../utils -I../keytuples_extracter -I../ngrams_generator -I../classifier -I../language_detector -I../text_classifier -I../gist_maker -I../profiler -c profile.cc
	@ar -r ${LIB_DIR}/libprofile_nif.a profile.o
	@ranlib ${LIB_DIR}/libprofile_nif.a
	@echo "done!"
	@echo "creating erlang library file libprofile_erl.so ..."
	@gcc -fPIC -shared -o ../../lib/libprofile_erl.so profile_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L ${LIB_DIR} -lprofile_nif -lprofiler -lgistmaker -lngramsgenerator -linagistapi -ltwitterapi -lutils -lscriptdetector -lcurlrequestmaker -ljsonparser -lcurl
	@echo "done!"
	@echo "compiling profile.erl"
	@erlc profile.erl
	@echo "done!"
