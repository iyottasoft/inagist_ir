SUBDIRS = script_detector utils stemmer keytuples_extracter ngrams_generator corpus_manager classifier language_classifier sentiment_analyser intent_finder trends_manager text_classifier gist_collector gist_maker profiler

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: clean

clean:
	@ for dir in $(SUBDIRS); do \
	    cd $$dir; make clean; cd ..; \
	  done
