include ../../Makefile.incl
PROJECT = keytuplesextracter
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL} -DI18N_ENABLED -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DTEXT_CLASSIFICATION_ENABLED #-DSCRIPT_DETECTION_ENABLED -DPROFANITY_CHECK_ENABLED -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DLANG_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../../extern/curl_request_maker -I../../extern/inagist_api -I../../extern/twitter_api -I../utils -I../script_detector -I../url_words_extracter

LIBS = -l${PROJECT} -lurlwordsextracter -lscriptdetector -lutils -ltwitterapi -linagistapi -lcurlrequestmaker -ljsonparser -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_keytuples_extracter #\
       ${BIN_DIR}/x_named_entities_extracter \
       ${BIN_DIR}/t_named_entities_manager

OBJS = keytuples_extracter.o named_entities_manager.o keytuples_config.o

all: ${LIB} ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_keytuples_extracter: t_keytuples_extracter.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keytuples_extracter
	@echo "done!"

${BIN_DIR}/x_named_entities_extracter: x_named_entities_extracter.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/x_named_entities_extracter
	@echo "done!"

${BIN_DIR}/t_named_entities_manager: t_named_entities_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} -DDEBUG=${DEBUG_LEVEL} -DNAMED_ENTITIES_ENABLED $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_named_entities_manager
	@echo "done!"

# DO NOT DELETE THIS LINE
