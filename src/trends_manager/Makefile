PROJECT = trendsmanager
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated -D_cplusplus
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../keywords_extracter 

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lkeywordsextracter -lstemmer -ljsonparser -lcurlrequestmaker -ltwitcurl -lcurl

LIB_DIR = ../../lib
BIN_DIR = ../../bin


LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_trends_manager

OBJS = trends_manager.o 

all: ${LIB} ${BINS} trends
	cp trends_generater.py ${BIN_DIR}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@rm ${BIN_DIR}/trends_generater.py
	@rm trends.beam
	@rm trends.so
	@echo "done!"

${LIB}: ${OBJS}
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_trends_manager: t_trends_manager.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_trends_manager
	@echo "done!"

trends:
	gcc -fPIC -shared -o trends.so trends.c -I/usr/local/lib/erlang/usr/include/ -I../keywords_extracter -L../../lib -ltrendsmanager -lkeywordsextracter -lstdc++ -lei -lerl_interface  -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib

# DO NOT DELETE THIS LINE
