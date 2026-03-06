SUBDIRS = src

# Phony target for building all subdirectories
.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

# Rule to enter each subdirectory and run make
$(SUBDIRS):
	$(MAKE) -C $@

# Example of a 'clean' target that also recurses
clean:
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir clean; \
	done
