# etch

## run

`dune exec ./etch.bc`

## test

- `dune runtest`
- `dune promote` to accept snapshot changes
- `dune runtest --auto-promote` to do both

- `Make coverage` will try to use bisect_ppx to make
and open a coverage report. You may need to install bisect_ppx,
I wans't sure how to make it installed automatically.

## format 

`dune build @fmt --auto-promote`

