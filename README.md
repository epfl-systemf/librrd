# Automatic layout of railroad diagrams

Thanks for taking a look at our code supplement!

## Requirements

- To run the scripts: Racket version 8.10 or greater (`apt install racket` on Ubuntu).
- To reproduce figures exactly: Linux Libertine fonts (`apt install fonts-linuxlibertine` on Ubuntu).

This code supplement was tested on Ubuntu 24.04.2 LTS.

## Supplement contents

- `librrd.rkt`: Railroad layout library
- `rrd`: Command-line interface
- `rrg-gui`: Graphical interface
- `oopsla25`: Reproduction script for the paper's figures

## Command line usage

Use `./rrd filename.rrd` to render the diagram contained in `filename.rrd`, creating `filename.rrd.svg`.
Add `--width` to control the width of the layout (try `--width 0` for min-content; `--width -1` for max-content).  For example:

```
$ ./rrd --width 400 sql-constraint.rrd
cpu time: 80 real time: 80 gc time: 4
```

The last line shows performance statistics for the layout pass (in ms, as per Racket's `(time)` function).

The syntax of `.rrd` files follows the paper:

```
terminal:       "l"
nonterminal:    "[l]"
epsilon:        ()
sequence:       (d...)
positive stack: (+ d d...)
negative stack: (- d d)
```

Use `./rrd -h` for help on additional flags.

## GUI usage

Use `./rrd-gui` to launch a graphical diagram explorer.  Input a diagram in the top-right box, then click “submit” to compute breaks and render the resulting layout.  Adjust the target sized by clicking and dragging on the display pane (left).

## Reproducing the paper's figures

Run `./oopsla25.rkt` to reproduce the figures shown in the paper.  The script creates on PDF per figure, in the current directory.
