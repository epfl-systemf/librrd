rrd.zip: README.md librrd.rkt rrd sql-constraint.rrd rrd-gui ../figures/generated/oopsla25.rkt
	rm -f $@
	zip $@ --junk-paths $^

.PHONY: rrd.zip
