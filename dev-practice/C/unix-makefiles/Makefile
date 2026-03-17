SUBDIRS = src

# Phony target for building all subdirectories
.PHONY: subdirs $(SUBDIRS)

# Explicitly setting .DEFAULT_GOAL for clarity; otherwise, the first defined target is the default
.DEFAULT_GOAL := subdirs

# Rule to enter each subdirectory and run make
$(SUBDIRS):
	$(MAKE) -C $@

subdirs: $(SUBDIRS)

# Example of a 'clean' target that also recurses
clean:
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir clean; \
	done
