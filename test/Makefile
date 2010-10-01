SUBDIRS = nif utf8 livemint 

subdirs:
	@ for dir in $(SUBDIRS); do \
	    $(MAKE) -C $$dir; \
	  done

clean:
	@ for dir in $(SUBDIRS); do \
	    cd $$dir; make clean; cd ..; \
	  done
