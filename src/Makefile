SUBDIRS = curl_request_maker twitter_api inagist_api script_detector utils stemmer keytuples_extracter ngrams_generator classifier language_detector sentiment_analyser intent_finder trends_manager channel_manager text_classifier gist_maker profiler

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: clean

clean:
	@ for dir in $(SUBDIRS); do \
	    cd $$dir; make clean; cd ..; \
	  done
