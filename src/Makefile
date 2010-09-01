SUBDIRS = curl_request_maker keywords_extracter trends_manager

subdirs:
	@ for dir in $(SUBDIRS); do \
	    $(MAKE) -C $$dir; \
	  done

clean:
	@ for dir in $(SUBDIRS); do \
	    cd $$dir; make clean; cd ..; \
	  done
