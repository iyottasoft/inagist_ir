# specify directory paths in Makefile.incl
include ../../Makefile.incl

PROJECT = erl_nif
PROJECTFLAGS = -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

# erlang related
ERL_INCLUDE_DIR = -I/usr/local/lib/erlang/usr/include/ -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include
ERL_LIB_DIR = -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib
ERL_LIBS = -lstdc++ -lei -lerl_interface

# local directories
LOCAL_LIB_DIR = ./lib
CPP_LINK_DIR = ../cpp_i/lib
CPP_INCLUDE_DIR = ../cpp_i

#naming convention for libraries.
# 1. the same noun will be used in the cpp class, erl module and all library names, unless an exception is documented (for historical reasons!)
# 2. always use underscores between words but not between 'lib' and the noun
# 3. X_api means, you access X using that api
# 4. Y_nif means, it has native implementations of Y

KEYTUPLES = -I${CPP_SRC_DIR}/keytuples_extracter ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc
KEYTUPLES_OBJS = keytuples_config.o keytuples_extracter.o
KEYTUPLES_FLAGS = -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED

CLASSIFIER =  -I${CPP_SRC_DIR}/corpus_manager -I${CPP_SRC_DIR}/ngrams_generator -I${CPP_SRC_DIR}/classifier ${CPP_SRC_DIR}/classifier/classifier_config.cc ${CPP_SRC_DIR}/classifier/naive_bayes_classifier.cc ${CPP_SRC_DIR}/classifier/classifier.cc
CLASSIFIER_OBJS = classifier_config.o naive_bayes_classifier.o classifier.o

LANG =  -I${CPP_SRC_DIR}/language_classifier ${CPP_SRC_DIR}/language_classifier/language_classifier.cc
LANG_OBJS = language_classifier.o
LANG_FLAGS = -DLANG_ENABLED

TEXT_CLASSIFICATION = -I${CPP_SRC_DIR}/text_classifier ${CPP_SRC_DIR}/text_classifier/text_classifier.cc 
TEXT_CLASSIFICATION_OBJS = text_classifier.o
TEXT_CLASSIFICATION_FLAGS = -DTEXT_CLASSIFICATION_ENABLED

GIST_COMPONENTS = -I${CPP_SRC_DIR}/gist_collector ${CPP_SRC_DIR}/gist_collector/gist_collector.cc gist_collector_cppi.cc \
                  ${KEYTUPLES} \
                  ${CLASSIFIER} \
                  ${TEXT_CLASSIFICATION} \
                  ${LANG}
GIST_OBJS = gist_collector.o gist_collector_cppi.o \
            ${KEYTUPLES_OBJS} \
            ${CLASSIFIER_OBJS} \
            ${TEXT_CLASSIFICATION_OBJS} \
            ${LANG_OBJS}
GIST_FLAGS = ${KEYTUPLES_FLAGS} ${LANG_FLAGS} ${TEXT_CLASSIFICATION_FLAGS}

all: stem keytuples intent trends lang text_class gist channels gist_maker profiler
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o *.beam ${EXPORT_LIB_DIR}/*.so
	@echo "done!"

gist: gist_collector
	@echo "done!"

gist_collector:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libgist_collector.so ..."
	@gcc -Wextra -fPIC -shared ${GIST_FLAGS} -o ${EXPORT_LIB_DIR}/libgist_collector.so gist_collector_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lgist_collector_cppi -ltwitter_api_cppi -L${CPP_LIB_DIR} -lngramsgenerator -lcorpusmanager -ltwitterapi -linagistapi -lutils -lscriptdetector -lcurlrequestmaker -ljsonparser -lcurl
	@echo "done!"
	@echo "compiling gist_collector.erl"
	@erlc gist_collector.erl
	@echo "done!"

stem: stemmer
	@echo "done!"

stemmer:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libstemmer.so ..."
	@gcc -Wextra -fPIC -shared -o ${EXPORT_LIB_DIR}/libstemmer.so stemmer_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I${CPP_INCLUDE_DIR}/json_parser -I${CPP_INCLUDE_DIR}/porter_stemmer -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/stemmer -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/curl_request_maker -I${CPP_SRC_DIR}/twitter_api -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lstemmer_cppi -L${CPP_LIB_DIR} -lstemmer -lporterstemmer -ltwitter_api_cppi -ltwitterapi -lutils -lscriptdetector -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling stemmer.erl"
	@erlc stemmer.erl
	@echo "done!"

keytuples: keytuples_extracter
	@echo "done!"

keytuples_extracter:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libkeytuples_extracter_enif.so ..."
	@gcc -Wextra -fPIC -shared -o ${EXPORT_LIB_DIR}/libkeytuples_extracter_enif.so keytuples_extracter_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lkeytuples_extracter_cppi -L${CPP_LIB_DIR} -lutils -lscriptdetector -ltwitter_api_cppi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling keytuples_extracter.erl"
	@erlc keytuples_extracter.erl
	@echo "done!"

intent: intent_finder
	@echo "done!"

intent_finder:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libintent_finder_enif.so ..."
	@gcc -Wextra -fPIC -shared -o ${EXPORT_LIB_DIR}/libintent_finder_enif.so intent_finder_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lintent_finder_cppi -L${CPP_LIB_DIR} -lutils -lscriptdetector -ltwitter_api_cppi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling intent_finder.erl"
	@erlc intent_finder.erl
	@echo "done!"

sentiment: sentiment_analyser
	@echo "done!"

sentiment_analyser:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libsentiment_enif.o ..."
	@gcc -Wextra -fPIC -shared -o ${EXPORT_LIB_DIR}/libsentiment_enif.so sentiment_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lsentiment_cppi -L${CPP_LIB_DIR} -lutils -lscriptdetector -ltwitter_api_cppi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling sentiment.erl"
	@erlc sentiment.erl
	@echo "done!"

trends: trends_manager
	@echo "done!"

trends_manager:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libtrends_manager_enif.so ..."
	@gcc -Wextra -fPIC -shared -o ${EXPORT_LIB_DIR}/libtrends_manager_enif.so trends_manager_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -ltrends_manager_cppi -L${CPP_LIB_DIR} -ltrendsmanager -lkeytuplesextracter -lutils -lscriptdetector -ltwitter_api_cppi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling trends_manager.erl"
	@erlc trends_manager.erl
	@echo "done!"

lang: language_classifier
	@echo "done!"

language_classifier:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/liblanguage_classifier_enif.so ..."
	@gcc -Wextra -fPIC -shared -o ${EXPORT_LIB_DIR}/liblanguage_classifier_enif.so language_classifier_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -llanguage_classifier_cppi -L${CPP_LIB_DIR} -lngramsgenerator -lcorpusmanager -lutils -lscriptdetector -ltwitter_api_cppi -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling language_classifier.erl"
	@erlc language_classifier.erl
	@echo "done!"

text_class: text_classifier
	@echo "done!"

text_classifier:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libtext_classifier_enif.so ..."
	@gcc -Wextra -fPIC -shared -o ${EXPORT_LIB_DIR}/libtext_classifier_enif.so text_classifier_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -ltext_classifier_cppi -L${CPP_LIB_DIR} -lclassifier -lutils -lscriptdetector -ltwitter_api_cppi -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling text_classifier.erl"
	@erlc text_classifier.erl
	@echo "done!"

channels: channel_manager
	@echo "done!"

channel_manager:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libchannel_manager_enif.so ..."
	@gcc -Wextra -fPIC -shared -o ${EXPORT_LIB_DIR}/libchannel_manager_enif.so channel_manager_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lchannel_manager_cppi -L${CPP_LIB_DIR} -lchannelmanager -lutils -lscriptdetector -ltwitter_api_cppi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling channel_manager.erl"
	@erlc channel_manager.erl
	@echo "done!"

gist_maker:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libgist_maker_enif.so ..."
	@gcc -Wextra -fPIC -shared -o ${EXPORT_LIB_DIR}/libgist_maker_enif.so gist_maker_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lgist_maker_cppi -L${CPP_LIB_DIR} -lngramsgenerator -lutils -lscriptdetector -ltwitter_api_cppi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl -DI18N_ENABLED -DPROFANITY_CHECK_ENABLED -DSCRIPT_DETECTION_ENABLED -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DLANG_ENABLED -DLOCATION_ENABLED
	@echo "done!"
	@echo "compiling gist_maker.erl"
	@erlc gist_maker.erl
	@echo "done!"

profiler:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libprofiler_enif.so ..."
	@gcc -Wextra -fPIC -shared -o ${EXPORT_LIB_DIR}/libprofiler_enif.so profiler_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lprofiler_cppi -L${CPP_LIB_DIR} -lprofiler -lgist_collector -lngramsgenerator -lcorpusmanager -linagistapi -ltwitterapi -lutils -lscriptdetector -lcurlrequestmaker -ljsonparser -lcurl
	@echo "done!"
	@echo "compiling profiler.erl"
	@erlc profiler.erl
	@echo "done!"
