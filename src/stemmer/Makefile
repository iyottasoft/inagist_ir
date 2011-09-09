include ../../Makefile.incl
PROJECT = stemmer
PROJECTFLAGS = -D_CPLUSPLUS -DDEBUG=${DEBUG_LEVEL}
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../include/porter_stemmer -I../../include/utfcpp -I../utils -I../curl_request_maker -I../../extern/twitter_api -I../script_detector

LIBS = -l${PROJECT} -lscriptdetector -ltwitterapi -ljsonparser -lcurlrequestmaker -lutils -lporterstemmer -lcurl

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_stemmer

OBJS = stemmer.o

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

${BIN_DIR}/t_stemmer: t_stemmer.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_stemmer
	@echo "done!"

# DO NOT DELETE THIS LINE
