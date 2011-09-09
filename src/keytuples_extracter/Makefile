include ../../Makefile.incl
PROJECT = keytuplesextracter
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL} -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DLANG_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../../extern/curl_request_maker -I../../extern/inagist_api -I../../extern/twitter_api -I../utils -I../script_detector

LIBS = -l${PROJECT} -lscriptdetector -lutils -ltwitterapi -linagistapi -lcurlrequestmaker -ljsonparser -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_keytuples_extracter \
       ${BIN_DIR}/t_named_entities_manager \
       ${BIN_DIR}/x_named_entities_extracter

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
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_named_entities_manager
	@echo "done!"

${BIN_DIR}/t_keytuples_from_twitter: t_keytuples_from_twitter.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keytuples_from_twitter
	@echo "done!"

${BIN_DIR}/t_keytuples_from_inagist: t_keytuples_from_inagist.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keytuples_from_inagist
	@echo "done!"

# DO NOT DELETE THIS LINE
