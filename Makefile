SUBDIRS = include extern src api test

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	@ for dir in $(SUBDIRS); do \
	    cd $$dir; make clean; cd ..; \
	  done
