# etch 

A type checker that works via rewriting.

An ill-typed expression is rewritten to an error:

    let x := 2 in x + true
    ----------------
    (let x := Int in (x + true))
    ----------------
    (Int + true)
    ----------------
    (Int + Bool)
    ----------------
    (Error: expected Bool, got Int)

A well-typed expression is rewritten to its type:

    ((fun x : Int := (2 = x)) 1)
    ----------------
    ((fun _ : Int := (2 = Int)) 1)
    ----------------
    ((fun _ : Int := (Int = Int)) 1)
    ----------------
    ((fun _ : Int := Bool) 1)
    ----------------
    ((Int -> Bool) 1)
    ----------------
    ((Int -> Bool) Int)
    ----------------
    Bool

See more examples in ./test/check_test.ml.
The main logic is in ./lib/work.ml. The type-checker does not use any context or environment.

## test

- `dune runtest`
- `dune promote` to accept snapshot changes
- `dune runtest --auto-promote` to do both

- `Make coverage` will try to create and open a coverage report.
You may need to `opam install bisect_ppx`.

## format 

`dune build @fmt --auto-promote`

## idea

Loosely Inspired by Kuan, G., MacQueen, D., & Findler, R. B. (2007, March).
[A rewriting semantics for type inference](https://link.springer.com/content/pdf/10.1007/978-3-540-71316-6_29.pdf). In European Symposium on Programming (pp. 426-440). Springer, Berlin, Heidelberg. 

There's no polymorphism here yet, so this repo is missing anything corresponding
to the meat of the paper, but the code here does proide a way to (as they put it)
"...explore a completely different way to think about the type checking process".

## next

If I ever come back to this repo,
["Local Type Inference"](https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf) could be a good fit for a rewriting-style-type-checker.

