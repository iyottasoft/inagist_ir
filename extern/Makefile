SUBDIRS = curl_request_maker twitter_api inagist_api #amazon_api

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: clean

clean:
	@ for dir in $(SUBDIRS); do \
	    cd $$dir; make clean; cd ..; \
	  done
