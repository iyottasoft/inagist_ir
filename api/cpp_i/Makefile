# specify directory paths in Makefile.incl
include ../../Makefile.incl

PROJECT = cppi
PROJECTFLAGS = -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

# local directories
LOCAL_LIB_DIR = ./lib

#naming convention for libraries.
# 1. the same noun will be used in the cpp class and all library names, unless an exception is documented (for historical reasons!)
# 2. always use underscores between words but not between 'lib' and the noun
# 3. X_api means, you access X using that api
# 4. Y_cppi means, it has native implementations of Y

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

all: tweets stem keytuples intent trends lang text_class gist gist_maker profiler
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o *.beam ${LOCAL_LIB_DIR}/*.a ${EXPORT_LIB_DIR}/*.so
	@echo "done!"

gist: gist_cppi
	@echo "done!"

gist_cppi:
	@echo "creating library ${LOCAL_LIB_DIR}/libgist_collector_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared ${GIST_FLAGS} -c ${GIST_COMPONENTS} -I${CPP_INCLUDE_DIR}/utfcpp -I${EXTERN_DIR}/twitter_api -I${EXTERN_DIR}/inagist_api -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/utils
	@ar -r ${LOCAL_LIB_DIR}/libgist_collector_cppi.a ${GIST_OBJS}
	@ranlib ${LOCAL_LIB_DIR}/libgist_collector_cppi.a
	@rm ${GIST_OBJS}
	@echo "done!"

tweets: twitter_api_cppi
	@echo "done!"

twitter_api_cppi:
	@echo "creating library ${LOCAL_LIB_DIR}/libtwitter_api_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c twitter_api_cppi.cc -I${EXTERN_DIR}/twitter_api
	@ar -r ${LOCAL_LIB_DIR}/libtwitter_api_cppi.a twitter_api_cppi.o
	@ranlib ${LOCAL_LIB_DIR}/libtwitter_api_cppi.a
	@echo "done!"

stem: stemmer
	@echo "done!"

stemmer:
	@echo "creating library ${LOCAL_LIB_DIR}/libstemmer_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c stemmer_cppi.cc -I${CPP_INCLUDE_DIR}/porter_stemmer -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/stemmer
	@ar -r ${LOCAL_LIB_DIR}/libstemmer_cppi.a stemmer_cppi.o
	@ranlib ${LOCAL_LIB_DIR}/libstemmer_cppi.a
	@echo "done!"

keytuples: keytuples_extracter
	@echo "done!"

keytuples_extracter:
	@echo "creating library ${LOCAL_LIB_DIR}/libkeytuples_extracter_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -c ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc keytuples_extracter_cppi.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/keytuples_extracter
	@ar -r ${LOCAL_LIB_DIR}/libkeytuples_extracter_cppi.a keytuples_config.o keytuples_extracter.o keytuples_extracter_cppi.o
	@rm keytuples_config.o keytuples_extracter.o
	@ranlib ${LOCAL_LIB_DIR}/libkeytuples_extracter_cppi.a
	@echo "done!"

intent: intent_finder
	@echo "done!"

intent_finder:
	@echo "creating library ${LOCAL_LIB_DIR}/libintent_finder_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -DINTENT_ENABLED -c ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc intent_finder_cppi.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/keytuples_extracter
	@ar -r ${LOCAL_LIB_DIR}/libintent_finder_cppi.a keytuples_config.o keytuples_extracter.o intent_finder_cppi.o
	@ranlib ${LOCAL_LIB_DIR}/libintent_finder_cppi.a
	@rm keytuples_config.o keytuples_extracter.o
	@echo "done!"

sentiment: sentiment_analyser
	@echo "done!"

sentiment_analyser:
	@echo "creating library ${LOCAL_LIB_DIR}/libsentiment_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -DSENTIMENT_ENABLED -c ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc sentiment_analyser_cppi.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/keytuples_extracter
	@ar -r ${LOCAL_LIB_DIR}/libsentiment_cppi.a keytuples_config.o keytuples_extracter.o sentiment_analyser_cppi.o
	@ranlib ${LOCAL_LIB_DIR}/libsentiment_cppi.a
	@rm keytuples_config.o keytuples_extracter.o
	@echo "done!"

trends: trends_manager
	@echo "done!"

trends_manager:
	@echo "creating library ${LOCAL_LIB_DIR}/libtrends_manager_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c trends_manager_cppi.cc -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/keytuples_extracter -I${CPP_SRC_DIR}/trends_manager
	@ar -r ${LOCAL_LIB_DIR}/libtrends_manager_cppi.a trends_manager_cppi.o
	@ranlib ${LOCAL_LIB_DIR}/libtrends_manager_cppi.a
	@echo "done!"

lang: language_classifier
	@echo "done!"

language_classifier:
	@echo "creating library ${LOCAL_LIB_DIR}/liblanguage_classifier_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c -DLANG_ENABLED ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc ${CPP_SRC_DIR}/classifier/classifier_config.cc ${CPP_SRC_DIR}/classifier/naive_bayes_classifier.cc ${CPP_SRC_DIR}/classifier/classifier.cc ${CPP_SRC_DIR}/language_classifier/language_classifier.cc language_classifier_cppi.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${EXTERN_DIR}/twitter_api -I${EXTERN_DIR}/inagist_api -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/keytuples_extracter -I${CPP_SRC_DIR}/corpus_manager -I${CPP_SRC_DIR}/ngrams_generator -I${CPP_SRC_DIR}/classifier -I${CPP_SRC_DIR}/language_classifier
	@ar -r ${LOCAL_LIB_DIR}/liblanguage_classifier_cppi.a keytuples_config.o keytuples_extracter.o classifier_config.o naive_bayes_classifier.o classifier.o language_classifier.o language_classifier_cppi.o
	@ranlib ${LOCAL_LIB_DIR}/liblanguage_classifier_cppi.a
	@echo "done!"

text_class: text_classifier
	@echo "done!"

text_classifier:
	@echo "creating library ${LOCAL_LIB_DIR}/libtext_classifier_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c -DTEXT_CLASS_WORDS_ENABLED ${CPP_SRC_DIR}/keytuples_extracter/keytuples_config.cc ${CPP_SRC_DIR}/keytuples_extracter/keytuples_extracter.cc ${CPP_SRC_DIR}/text_classifier/text_classifier.cc text_classifier_cppi.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/keytuples_extracter -I${CPP_SRC_DIR}/corpus_manager -I${CPP_SRC_DIR}/ngrams_generator -I${CPP_SRC_DIR}/classifier -I${CPP_SRC_DIR}/text_classifier
	@ar -r ${LOCAL_LIB_DIR}/libtext_classifier_cppi.a keytuples_config.o keytuples_extracter.o text_classifier.o text_classifier_cppi.o
	@ranlib ${LOCAL_LIB_DIR}/libtext_classifier_cppi.a
	@echo "done!"

gist_maker:
	@echo "creating library ${LOCAL_LIB_DIR}/libgist_maker_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -c ${CPP_SRC_DIR}/gist_maker/gist_maker_config.cc ${CPP_SRC_DIR}/gist_maker/gist_maker.cc gist_maker_cppi.cc -I${CPP_INCLUDE_DIR}/utfcpp -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/script_detector -I${CPP_SRC_DIR}/corpus_manager -I${CPP_SRC_DIR}/ngrams_generator -I${CPP_SRC_DIR}/gist_maker -DI18N_ENABLED -DSCRIPT_DETECTION_ENABLED -DPROFANITY_CHECK_ENABLED -DKEYPHRASE_ENABLED -DNAMED_ENTITIES_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DLANG_ENABLED -DLOCATION_ENABLED
	@ar -r ${LOCAL_LIB_DIR}/libgist_maker_cppi.a gist_maker_config.o gist_maker.o gist_maker_cppi.o
	@rm gist_maker_config.o gist_maker.o
	@ranlib ${LOCAL_LIB_DIR}/libgist_maker_cppi.a
	@echo "done!"

profiler:
	@echo "creating library ${LOCAL_LIB_DIR}/libprofiler_cppi.a ..."
	@g++ ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -shared -DNAMED_ENTITIES_ENABLED -DKEYWORDS_ENABLED -DKEYPHRASE_ENABLED -DLANG_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DCLASS_CONTRIBUTORS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -I${CPP_SRC_DIR}/utils -I${CPP_SRC_DIR}/keytuples_extracter -I${CPP_SRC_DIR}/corpus_manager -I${CPP_SRC_DIR}/ngrams_generator -I${CPP_SRC_DIR}/classifier -I${CPP_SRC_DIR}/language_classifier -I${CPP_SRC_DIR}/text_classifier -I${CPP_SRC_DIR}/gist_collector -I${CPP_SRC_DIR}/profiler -c profiler_cppi.cc
	@ar -r ${LOCAL_LIB_DIR}/libprofiler_cppi.a profiler_cppi.o
	@ranlib ${LOCAL_LIB_DIR}/libprofiler_cppi.a
	@echo "done!"
