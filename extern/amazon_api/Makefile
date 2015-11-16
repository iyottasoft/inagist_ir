include ../../Makefile.incl
PROJECT = amazonapi
PROJECTFLAGS = #-DDEBUG
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I/usr/include/python2.6/

LIBS = -l${PROJECT}

PYLINK_DIR = -L/usr/lib/python2.6/config/
PYLIBS = -lpython2.6

LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_amazon_api

OBJS = amazon_api.o

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
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_amazon_api: t_amazon_api.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} ${PROJECTFLAGS} -fPIC $< ${LINK_DIR} ${LIBS} -L/usr/lib/python2.6/config -lpython2.6 -lm -o ${BIN_DIR}/t_amazon_api
	@echo "done!"

# DO NOT DELETE THIS LINE
