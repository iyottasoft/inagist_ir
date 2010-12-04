PROJECT = naiveguess
CXX = g++
CXXFLAGS = -Wall -g -Wno-deprecated #-DDEBUG
ARCHIVE = ar -r

INCLUDE_DIR = -I. -I/usr/include/c++/4.4/ -I../../src/ngrams_generator

LINK_DIR = -L../../lib

LIBS = -l${PROJECT} -lngramsgenerator -lscriptdetector -lutils

LIB_DIR = ../../lib
BIN_DIR = ../../bin


LIB = ${LIB_DIR}/lib${PROJECT}.so

BINS = ${BIN_DIR}/t_naive_guess

all: ${LIB} ${BINS}
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o ${BINS} ${LIB}
	@echo "done!"

${LIB}:
	@echo "creating library file ..."
	@${ARCHIVE} ${LIB} ${OBJS}
	@ranlib ${LIB} # this creates an index table
	@echo "done!"

.cc.o:
	@echo "compiling ..." $< "..."
	@${CXX} ${CXXFLAGS} -shared -fPIC -c ${INCLUDE_DIR} $<

${BIN_DIR}/t_naive_guess: t_naive_guess.o ${LIB}
	@echo "creating binary:" $@
	@${CXX} ${CXXFLAGS} $< ${LINK_DIR} ${LIBS} -o ${BIN_DIR}/t_naive_guess
	@echo "done!"

# DO NOT DELETE THIS LINE
