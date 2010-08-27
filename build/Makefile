PROJECT = trends
VERSION = 0.1

PROJECT_DIR = ${PROJECT}-${VERSION}

BIN_DIR = ../bin
SCRIPT_DIR = ../scripts
STATIC_DATA_DIR = ../data/static_data

BINS = ${BIN_DIR}/x_keywords_extract \
       ${BIN_DIR}/trends_generater.py

SCRIPTS = ${SCRIPT_DIR}/init.keywords_extracter.tcsh \
          ${SCRIPT_DIR}/init.trends_generater.tcsh

STATIC_DATA_FILES = ${STATIC_DATA_DIR}/stopwords.txt \
                    ${STATIC_DATA_DIR}/dictionary.txt

dist:
	@echo "making distribution tarball ..."
	@mkdir ${PROJECT_DIR}
	@mkdir ${PROJECT_DIR}/bin
	@cp ${BINS} ${PROJECT_DIR}/bin
	@mkdir ${PROJECT_DIR}/scripts
	@cp ${SCRIPTS} ${PROJECT_DIR}/scripts
	@mkdir -p ${PROJECT_DIR}/data/static_data
	@cp ${STATIC_DATA_FILES} ${PROJECT_DIR}/data/static_data
	@mkdir ${PROJECT_DIR}/log
	@mkdir ${PROJECT_DIR}/configs
	@cp ./configure ${PROJECT_DIR}
	@chmod +x ${PROJECT_DIR}/configure
	@touch ${PROJECT_DIR}/README
	tar -zcvf ${PROJECT_DIR}.tar.gz ${PROJECT_DIR}
	@echo "done."

clean:
	@echo "cleaning ..."
	rm -fr ${PROJECT_DIR} ${PROJECT_DIR}.tar.gz
	@echo "done."
