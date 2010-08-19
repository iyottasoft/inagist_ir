PROJECT = keywordsextracter
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/stemmer/ -I../../include/twitcurl -I../../include/json_parser -I../curl_request_maker

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lstemmer -ljsonparser -lcurlrequestmaker -ltwitcurl -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin


LIB = ${LIB_DIR}/lib${PROJECT}.a

BINS = ${BIN_DIR}/t_keywords_extract \
       ${BIN_DIR}/t_keywords_manager \
       ${BIN_DIR}/t_keywords_from_twitter \
       ${BIN_DIR}/t_keywords_from_inagist

OBJS = keywords_extract.o keywords_manager.o

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
	@${CXX} ${CXXFLAGS} -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_keywords_extract: t_keywords_extract.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keywords_extract
	@echo "done!"

${BIN_DIR}/t_keywords_manager: t_keywords_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keywords_manager
	@echo "done!"

${BIN_DIR}/t_keywords_from_twitter: t_keywords_from_twitter.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keywords_from_twitter
	@echo "done!"

${BIN_DIR}/t_keywords_from_inagist: t_keywords_from_inagist.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_keywords_from_inagist
	@echo "done!"

# DO NOT DELETE THIS LINE
