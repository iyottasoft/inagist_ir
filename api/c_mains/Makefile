# specify directory paths in Makefile.incl
include ../../Makefile.incl

PROJECT = c_mains

CPP_INCLUDE_DIR = ../cpp_i
CPP_LINK_DIR = ../cpp_i/lib

# local directories
LOCAL_LIB_DIR = ./lib
LOCAL_BIN_DIR = ./bin

all: gist_collector_multi gist_collector gist_maker profiler
	@echo "build done!"

clean:
	@echo "cleaning libraries ..."
	@rm *.o *.beam ${LOCAL_LIB_DIR}/*.a ${EXPORT_LIB_DIR}/*.so
	@echo "done!"

gist_collector_multi:
	@echo "creating binary ${LOCAL_BIN_DIR}/t_gist_collector_multi ..."
	@gcc -g -Wall t_gist_collector_multi.c -o ${LOCAL_BIN_DIR}/t_gist_collector_multi -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lstdc++ -ltwitter_api_cppi -lgist_collector_cppi -L${CPP_LIB_DIR} -lscriptdetector -lngramsgenerator -lcorpusmanager -lutils -lpthread -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DLANG_ENABLED
	@echo "done!"

gist_collector:
	@echo "creating binary ${LOCAL_BIN_DIR}/t_gist_collector ..."
	@gcc -g -Wall t_gist_collector.c -o ${LOCAL_BIN_DIR}/t_gist_collector -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lstdc++ -ltwitter_api_cppi -lgist_collector_cppi -L${CPP_LIB_DIR} -lscriptdetector -lngramsgenerator -lcorpusmanager -lutils -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DLANG_ENABLED
	@echo "done!"

gist_maker:
	@echo "creating binary ${LOCAL_BIN_DIR}/t_gist_maker ..."
	@gcc -g -Wall t_gist_maker.c -o ${LOCAL_BIN_DIR}/t_gist_maker -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -lstdc++ -ltwitter_api_cppi -lgist_maker_cppi -L${CPP_LIB_DIR} -lngramsgenerator -lscriptdetector -lutils -DI18N_ENABLED -DSCRIPT_DETECTION_ENABLED -DPROFANITY_CHECK_ENABLED -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DLANG_ENABLED -DLOCATION_ENABLED
	@echo "done!"

profiler:
	@echo "creating binary ${LOCAL_BIN_DIR}/t_profiler ..."
	@gcc -g -Wall t_profiler.c -o ${LOCAL_BIN_DIR}/t_profiler -I. -I${CPP_INCLUDE_DIR} -L${CPP_LINK_DIR} -ltwitter_api_cppi -lprofiler_cppi -L${CPP_LIB_DIR} -lprofiler -lngramsgenerator -lscriptdetector -lutils -L${EXTERN_DIR} -ltwitterapi -linagistapi -ljsonparser -lcurlrequestmaker -lcurl -lstdc++ -DI18N_ENABLED -DSCRIPT_DETECTION_ENABLED -DPROFANITY_CHECK_ENABLED -DNAMED_ENTITIES_ENABLED -DKEYPHRASE_ENABLED -DKEYWORDS_ENABLED -DINTENT_ENABLED -DSENTIMENT_ENABLED -DTEXT_CLASSIFICATION_ENABLED -DLANG_ENABLED -DRECSYS_ENABLED #-DLOCATION_ENABLED
	@echo "done!"
