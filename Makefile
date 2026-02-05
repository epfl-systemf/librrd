claim_1_names := default 300 300-global center-align 900-right-justify 900-end-justify 900-flex-absorb gap-50 all
claim_1_files := $(addprefix cli/examples/json-number-, $(addsuffix .svg, $(claim_1_names)))

cli/examples/json-number-%.svg: cli/examples/json-number.rrd cli/examples/json-number-%.rrdargs cli/examples/json-number-%.rrdcss cli/examples/json.css
	node cli/cli.js -o $@ -r cli/examples/json.css -L $(word 3,$^) $< $(file < $(word 2,$^))

cli/examples/%.rrdargs:
	echo 1 > $@
cli/examples/%.rrdcss:
	touch $@

.PHONY: claim-1 clean

claim-1: $(claim_1_files)

clean:
	-rm -r target/
	-rm cli/*.js cli/*.js.map gui/*.js gui/*.js.map
	find cli/examples -name "*.svg" -execdir rm {} +
