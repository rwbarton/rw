rw.rc: rw.conf
	cp rw.conf rw.rc

include/enums.h: crawl-debug/crawl-ref/source/crawl
	dwarf2enum --dedupe $< > $@ || rm $@
