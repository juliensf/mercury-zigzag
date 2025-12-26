include ../Make.options

.PHONY: default
default:
	$(MMC) --make libmercury_zigzag

.PHONY: rebuild
rebuild:
	$(MMC) --rebuild libmercury_zigzag

.PHONY: install
install:
	$(MMC) --make libmercury_zigzag.install

tags: $(wildcard *.m)
	mtags $^

.PHONY: clean
clean:
	$(MMC) --make mercury_zigzag.clean

.PHONY: realclean
realclean:
	$(MMC) --make mercury_zigzag.realclean
	/bin/rm -rf Mercury
	/bin/rm -f *.err *.mh
	/bin/rm -f tags
