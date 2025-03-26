#!/usr/bin/env bash
build=_zip/
targetdir=$build/rrd/

rm -fr "$build"
mkdir -p "$targetdir"

cp README.md librrd.rkt rrd sql-constraint.rrd rrd-gui ../figures/generated/oopsla25.rkt "$targetdir"
cd $build; zip -r rrd.zip rrd/
