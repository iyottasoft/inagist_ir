SUBDIRS = curl_request_maker twitter_api inagist_api script_detector utils stemmer keytuples_extracter ngrams_generator classifier language_detector sentiment_analyser trends_manager follow_analyser channel_manager text_classifier gist_maker erlang_nif

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: clean

clean:
	@ for dir in $(SUBDIRS); do \
	    cd $$dir; make clean; cd ..; \
	  done
