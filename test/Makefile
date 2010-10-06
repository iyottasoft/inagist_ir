SUBDIRS = nif utf8 dashboard

subdirs:
	@ for dir in $(SUBDIRS); do \
	    $(MAKE) -C $$dir; \
	  done

clean:
	@ for dir in $(SUBDIRS); do \
	    cd $$dir; make clean; cd ..; \
	  done
