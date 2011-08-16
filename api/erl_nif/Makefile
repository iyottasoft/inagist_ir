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

LANG =  -I${CPP_SRC_DIR}/language_detector ${CPP_SRC_DIR}/language_detector/language_detector.cc
LANG_OBJS = language_detector.o
LANG_FLAGS = -DLANG_ENABLED

TEXT_CLASSIFICATION = -I${CPP_SRC_DIR}/text_classifier ${CPP_SRC_DIR}/text_classifier/text_classifier.cc 
TEXT_CLASSIFICATION_OBJS = text_classifier.o
TEXT_CLASSIFICATION_FLAGS = -DTEXT_CLASSIFICATION_ENABLED

GIST_COMPONENTS =  -I${CPP_SRC_DIR}/gist_maker ${CPP_SRC_DIR}/gist_maker/gist_maker.cc gist_maker_eapi.cc \
                  ${KEYTUPLES} #\
                  ${CLASSIFIER} \
                  ${TEXT_CLASSIFICATION} \
                  ${LANG}
GIST_OBJS = gist_maker.o gist_maker_eapi.o \
            ${KEYTUPLES_OBJS} #\
            ${CLASSIFIER_OBJS} \
            ${TEXT_CLASSIFICATION_OBJS} \
            ${LANG_OBJS}
GIST_FLAGS = ${KEYTUPLES_FLAGS}# ${LANG_FLAGS} ${TEXT_CLASSIFICATION_FLAGS}

all: tweets stem named_entities keytuples intent trends lang text_class gist channels profile
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o *.beam ${LOCAL_LIB_DIR}/*.a ${EXPORT_LIB_DIR}/*.so
	@echo "done!"

gist: gist_nif gist_erl
	@echo "done!"

gist_nif:
	@echo "creating library ${LOCAL_LIB_DIR}/libgist_maker_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared ${GIST_FLAGS} -c ${GIST_COMPONENTS} -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/twitter_api -I${CPP_SRC_DIR}/inagist_api -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/utils
	@ar -r ${LOCAL_LIB_DIR}/libgist_maker_nif.a ${GIST_OBJS}
	@ranlib ${LOCAL_LIB_DIR}/libgist_maker_nif.a
	@echo "done!"

gist_erl:
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libgist_maker.so ..."
	@gcc -fPIC -shared ${GIST_FLAGS} -o ${EXPORT_LIB_DIR}/libgist_maker.so gist_maker_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LOCAL_LIB_DIR} -lgist_maker_nif -L${CPP_LINK_DIR} -lngramsgenerator -lcorpusmanager -ltweets -ltwitterapi -linagistapi -lutils -lscriptdetector -lcurlrequestmaker -ljsonparser -lcurl
	@echo "done!"
	@echo "compiling gist_maker.erl"
	@erlc gist_maker.erl
	@echo "done!"

test_gist:
	gcc -g t_test_gist.c -o ../../bin/t_test_gist -I. -lstdc++ -L${LOCAL_LIB_DIR} -lgist_maker_nif -L${CPP_LINK_DIR} -lngramsgenerator -lcorpusmanager -ltweets -ltwitterapi -linagistapi -lutils -lscriptdetector -lcurlrequestmaker -ljsonparser -lcurl
	@echo "done!"

tweets:
	@echo "creating library ${LOCAL_LIB_DIR}/libtweets.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c tweets.cc -I${CPP_SRC_DIR}/twitter_api
	@ar -r ${LOCAL_LIB_DIR}/libtweets.a tweets.o
	@ranlib ${LOCAL_LIB_DIR}/libtweets.a
	@echo "done!"

stem:
	@echo "creating library ${LOCAL_LIB_DIR}/libstemmer_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c stemmer_eapi.cc -I${CPP_INCLUDE_DIR}/porter_stemmer -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/stemmer
	@ar -r ${LOCAL_LIB_DIR}/libstemmer_nif.a stemmer_eapi.o
	@ranlib ${LOCAL_LIB_DIR}/libstemmer_nif.a
	@echo "done!"
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libstemmer.so ..."
	@gcc -fPIC -shared -o ${EXPORT_LIB_DIR}/libstemmer.so stemmer_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I${CPP_INCLUDE_DIR}/json_parser -I${CPP_INCLUDE_DIR}/porter_stemmer -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/stemmer -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/curl_request_maker -I${CPP_SRC_DIR}/twitter_api -I. -L${LOCAL_LIB_DIR} -lstemmer_nif -L${CPP_LINK_DIR} -lstemmer -lporterstemmer -ltweets -ltwitterapi -lutils -lscriptdetector -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling stemmer.erl"
	@erlc stemmer.erl
	@echo "done!"

named_entities:
	@echo "creating library ${LOCAL_LIB_DIR}/libnamed_entities_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -DNAMED_ENTITIES_ENABLED -c ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc named_entities.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/keytuples_extracter
	@ar -r ${LOCAL_LIB_DIR}/libnamed_entities_nif.a keytuples_config.o keytuples_extracter.o named_entities.o
	@ranlib ${LOCAL_LIB_DIR}/libnamed_entities_nif.a
	@rm keytuples_config.o keytuples_extracter.o
	@echo "done!"
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libnamed_entities_erl.so ..."
	@gcc -fPIC -shared -o ${EXPORT_LIB_DIR}/libnamed_entities_erl.so named_entities_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LOCAL_LIB_DIR} -lnamed_entities_nif -L${CPP_LINK_DIR} -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling named_entities.erl"
	@erlc named_entities.erl
	@echo "done!"

keytuples:
	@echo "creating library ${LOCAL_LIB_DIR}/libkeytuples_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -c ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc keytuples.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/keytuples_extracter
	@ar -r ${LOCAL_LIB_DIR}/libkeytuples_nif.a keytuples_config.o keytuples_extracter.o keytuples.o
	@rm keytuples_config.o keytuples_extracter.o
	@ranlib ${LOCAL_LIB_DIR}/libkeytuples_nif.a
	@echo "done!"
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libkeytuples_erl.so ..."
	@gcc -fPIC -shared -o ${EXPORT_LIB_DIR}/libkeytuples_erl.so keytuples_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LOCAL_LIB_DIR} -lkeytuples_nif -L${CPP_LINK_DIR} -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling keytuples.erl"
	@erlc keytuples.erl
	@echo "done!"

intent:
	@echo "creating library ${LOCAL_LIB_DIR}/libintent_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -DINTENT_ENABLED -c ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc intent.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/keytuples_extracter
	@ar -r ${LOCAL_LIB_DIR}/libintent_nif.a keytuples_config.o keytuples_extracter.o intent.o
	@ranlib ${LOCAL_LIB_DIR}/libintent_nif.a
	@rm keytuples_config.o keytuples_extracter.o
	@echo "done!"
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libintent_erl.so ..."
	@gcc -fPIC -shared -o ${EXPORT_LIB_DIR}/libintent_erl.so intent_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LOCAL_LIB_DIR} -lintent_nif -L${CPP_LINK_DIR} -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling intent.erl"
	@erlc intent.erl
	@echo "done!"

sentiment:
	@echo "creating library ${LOCAL_LIB_DIR}/libsentiment_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -DSENTIMENT_ENABLED -c ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc sentiment.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/keytuples_extracter
	@ar -r ${LOCAL_LIB_DIR}/libsentiment_nif.a keytuples_config.o keytuples_extracter.o sentiment.o
	@ranlib ${LOCAL_LIB_DIR}/libsentiment_nif.a
	@rm keytuples_config.o keytuples_extracter.o
	@echo "done!"
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libsentiment_erl.o ..."
	@gcc -fPIC -shared -o ${EXPORT_LIB_DIR}/libsentiment_erl.so sentiment_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LOCAL_LIB_DIR} -lsentiment_nif -L${CPP_LINK_DIR} -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling sentiment.erl"
	@erlc sentiment.erl
	@echo "done!"

trends:
	@echo "creating library ${LOCAL_LIB_DIR}/libtrends_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c trends.cc -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/keytuples_extracter -I${CPP_SRC_DIR}/trends_manager
	@ar -r ${LOCAL_LIB_DIR}/libtrends_nif.a trends.o
	@ranlib ${LOCAL_LIB_DIR}/libtrends_nif.a
	@echo "done!"
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libtrends_erl.so ..."
	@gcc -fPIC -shared -o ${EXPORT_LIB_DIR}/libtrends_erl.so trends_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LOCAL_LIB_DIR} -ltrends_nif -L${CPP_LINK_DIR} -ltrendsmanager -lkeytuplesextracter -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling trends.erl"
	@erlc trends.erl
	@echo "done!"

lang:
	@echo "creating library ${LOCAL_LIB_DIR}/liblang_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c -DLANG_WORDS_ENABLED ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc ${CPP_SRC_DIR}/language_detector/language_detector.cc lang.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/keytuples_extracter -I${CPP_SRC_DIR}/corpus_manager -I${CPP_SRC_DIR}/ngrams_generator -I${CPP_SRC_DIR}/classifier -I${CPP_SRC_DIR}/language_detector
	@ar -r ${LOCAL_LIB_DIR}/liblang_nif.a keytuples_config.o keytuples_extracter.o language_detector.o lang.o
	@ranlib ${LOCAL_LIB_DIR}/liblang_nif.a
	@echo "done!"
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/liblang_erl.so ..."
	@gcc -fPIC -shared -o ${EXPORT_LIB_DIR}/liblang_erl.so lang_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LOCAL_LIB_DIR} -llang_nif -L${CPP_LINK_DIR} -lclassifier -lngramsgenerator -lcorpusmanager -lutils -lscriptdetector -ltweets -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling lang.erl"
	@erlc lang.erl
	@echo "done!"

text_class:
	@echo "creating library ${LOCAL_LIB_DIR}/libtext_class_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c -DTEXT_CLASS_WORDS_ENABLED ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc ${CPP_SRC_DIR}/text_classifier/text_classifier.cc text_class.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/keytuples_extracter -I${CPP_SRC_DIR}/corpus_manager -I${CPP_SRC_DIR}/ngrams_generator -I${CPP_SRC_DIR}/classifier -I${CPP_SRC_DIR}/text_classifier
	@ar -r ${LOCAL_LIB_DIR}/libtext_class_nif.a keytuples_config.o keytuples_extracter.o text_classifier.o text_class.o
	@ranlib ${LOCAL_LIB_DIR}/libtext_class_nif.a
	@echo "done!"
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libtext_class_erl.so ..."
	@gcc -fPIC -shared -o ${EXPORT_LIB_DIR}/libtext_class_erl.so text_class_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LOCAL_LIB_DIR} -ltext_class_nif -L${CPP_LINK_DIR} -lclassifier -lutils -lscriptdetector -ltweets -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling text_class.erl"
	@erlc text_class.erl
	@echo "done!"

channels:
	@echo "creating library ${LOCAL_LIB_DIR}/libchannels_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c channels.cc -I${CPP_SRC_DIR}/channel_manager
	@ar -r ${LOCAL_LIB_DIR}/libchannels_nif.a channels.o
	@ranlib ${LOCAL_LIB_DIR}/libchannels_nif.a
	@echo "done!"
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libchannels_erl.so ..."
	@gcc -fPIC -shared -o ${EXPORT_LIB_DIR}/libchannels_erl.so channels_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LOCAL_LIB_DIR} -lchannels_nif -L${CPP_LINK_DIR} -lchannelmanager -lutils -lscriptdetector -ltweets -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl
	@echo "done!"
	@echo "compiling channels.erl"
	@erlc channels.erl
	@echo "done!"

profile:
	@echo "creating library ${LOCAL_LIB_DIR}/libprofile_nif.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -DNAMED_ENTITIES_ENABLED -DKEYWORDS_ENABLED -DKEYPHRASE_ENABLED -DLANG_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DCLASS_CONTRIBUTORS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/keytuples_extracter -I${CPP_SRC_DIR}/corpus_manager -I${CPP_SRC_DIR}/ngrams_generator -I${CPP_SRC_DIR}/classifier -I${CPP_SRC_DIR}/language_detector -I${CPP_SRC_DIR}/text_classifier -I${CPP_SRC_DIR}/gist_maker -I${CPP_SRC_DIR}/profiler -c profile.cc
	@ar -r ${LOCAL_LIB_DIR}/libprofile_nif.a profile.o
	@ranlib ${LOCAL_LIB_DIR}/libprofile_nif.a
	@echo "done!"
	@echo "creating erlang library file ${EXPORT_LIB_DIR}/libprofile_erl.so ..."
	@gcc -fPIC -shared -o ${EXPORT_LIB_DIR}/libprofile_erl.so profile_enif.c ${ERL_INCLUDE_DIR} ${ERL_LIB_DIR} ${ERL_LIBS} -I. -L${LOCAL_LIB_DIR} -lprofile_nif -L${CPP_LINK_DIR} -lprofiler -lgistmaker -lngramsgenerator -lcorpusmanager -linagistapi -ltwitterapi -lutils -lscriptdetector -lcurlrequestmaker -ljsonparser -lcurl
	@echo "done!"
	@echo "compiling profile.erl"
	@erlc profile.erl
	@echo "done!"
