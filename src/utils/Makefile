include ../../Makefile.incl
PROJECT = utils
PROJECTFLAGS = -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/utfcpp -I../../extern/curl_request_maker -I../../extern/twitter_api -I../../extern/inagist_api -I../script_detector

LIBS = -l${PROJECT} -lscriptdetector -linagistapi -ltwitterapi -ljsonparser -lcurlrequestmaker -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_string_utils \
       ${BIN_DIR}/t_dictionary_set \
       ${BIN_DIR}/t_dictionary_map \
       ${BIN_DIR}/t_int_dictionary_map \
       ${BIN_DIR}/t_double_dictionary_map \
       ${BIN_DIR}/t_test_utils

OBJS = string_utils.o dictionary_set.o dictionary_map.o int_dictionary_map.o double_dictionary_map.o string_to_map_dictionary.o test_utils.o

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

${BIN_DIR}/t_string_utils: t_string_utils.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_string_utils
	@echo "done!"

${BIN_DIR}/t_dictionary_set: t_dictionary_set.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_dictionary_set
	@echo "done!"

${BIN_DIR}/t_dictionary_map: t_dictionary_map.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_dictionary_map
	@echo "done!"

${BIN_DIR}/t_int_dictionary_map: t_int_dictionary_map.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_int_dictionary_map
	@echo "done!"

${BIN_DIR}/t_double_dictionary_map: t_double_dictionary_map.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_double_dictionary_map
	@echo "done!"

${BIN_DIR}/t_test_utils: t_test_utils.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_test_utils
	@echo "done!"

# DO NOT DELETE THIS LINE
