PROJECT = scriptdetector
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated #-DDEBUG
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/twitcurl -I../../include/json_parser -I../curl_request_maker -I../../include/utfcpp

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -ljsonparser -lcurlrequestmaker -ltwitcurl -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin


LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_scripts_from_inagist \
       ${BIN_DIR}/t_scripts_from_twitter

OBJS = script_detector.o

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
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_scripts_from_inagist: t_scripts_from_inagist.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_scripts_from_inagist
	@echo "done!"

${BIN_DIR}/t_scripts_from_twitter: t_scripts_from_twitter.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_scripts_from_twitter
	@echo "done!"

# DO NOT DELETE THIS LINE
