# Benchmarking Effect handlers.

Currently supported libraries and languages are:

- `kk` (extension `.kk`), [Koka].
- `ml` (extension `.ml`), [multi-core OCaml][mcocaml].
- `hs` (extension `.mp.hs`), [Mp.Eff] Haskell library.
- `ev` (extension `.ev.hs`), [Ev.Eff] Haskell library.
- `hia` (extension `.hia.hs`), [Handers-in-Action][hia] Haskell library.
- `lh` (extension `.lh.c`), [libhandler] C library.
- `lmp` (extension `.lmp.c`), [libmprompt] C library.
- `base` (extension `base.ml`), a plain _baseline_ version
   without using effect handlers directly in OCaml.

And the variants without tail-resumptive optimization.

- `kknt` (extension `.nt.kk`), _Koka_.
- `hsnt` (extension `.nt.mp.hs`), _Mp.Eff_.
- `evnt` (extension `.nt.ev.hs`), _Ev.Eff_.
- `lhnt` (extension `.nt.lh.c`), _libhandler_.
- `lmpnt` (extension `.nt.lmp.c`), _libmprompt_.

The benchmark sources are in the [`src`](src) directory and consist of:

- `counter`: tight loop with state effect handler doing a `get` and `put` (tail-resumptive) operation per iteration (100M).
- `counter1`: as `counter` but with an unused reader handler in between. 
- `counter10`: as `counter` but with 10 unused reader handlers in between. 
- `mstate`: as `counter`, but using a monadic non-tail-resumptive state handler (and only 10M iterations).
- `nqueens`: the nqueens 12 problem using a multiple resumes to implement backtracking.

The benchmarks are small and targeted by design, and effect handler operations should dominate the run times. 
To make the benchmarks as "equal" as possible, we note:

- We use native ints when possible (`Int` in Haskell, `int` in ML and C, and `int32` in Koka)
- Koka uses sometimes `rcontrol` (raw control) to avoid running finally clauses automatically (in `mstate`)

A detailed description of the benchmarks and comparison
can be found in the following tech report:

- _Generalized Evidence Passing for Effect Handlers_, Ningning Xie and Daan Leijen, MS-TR-2021-5, Mar 2021, [(pdf)](https://www.microsoft.com/en-us/research/publication/generalized-evidence-passing-for-effect-handlers/)


Enjoy,
  Daan and Ningning


Note: the benchmarks come with no support, please submit any improvements as a PR :-)
If you add your own system to the benchmarks, or add more
benchmarks please add detailed installation instructions to the readme and 
submit it as a PR (or become a dev on the repo).



# Installing Prerequisites

This is tested on Ubuntu 20.04 and macOS Catalina. Follow the installation instructions carefully.


## Koka

Install [koka]:
```
$ curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh
$ koka --version
Koka 2.1.1, 17:46:03 Mar  8 2021 (ghc release version)
```


## Haskell

Install Ghc and Cabal:
```
$ sudo apt-get install ghc cabal      # on macOS: > brew install ghc cabal-install
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.6.5    # 8.10.4 is also good
```

Install the `primitive` and `eveff` package:
```
$ cabal update
$ cabal install primitive eveff    # use cabal install --lib on ghc 8.10+
$ cabal info primitive eveff | grep installed:
Versions installed: 0.7.1.0  # primitive
Versions installed: 0.1.0.0  # eveff
```

## Ev.Eff

Already installed as the package `eveff`


## Mp.Eff

Clone from the benchmark root directory (into the `mpeff` sub-directory).
```
$ git clone https://github.com/xnning/mpeff
```

## Handlers-in-Action

Tested with ghc 8.6.5.
Clone from the benchmark root directory (into the `effect-handlers` sub-directory).
```
$ git clone https://github.com/slindley/effect-handlers -b ghc865
$ cabal update
$ cabal install random pipes haskell-src-meta haskell-src-exts network
```
(again, use `cabal install --lib` on ghc 8.10+)

## Libhandler

Install [libhandler] from the benchmark root directory (into the `libhandler` sub-directory).
```bash
$ git clone https://github.com/koka-lang/libhandler
$ cd libhandler
```

Build the library:
```bash
libhandler$ ./configure
libhandler$ make depend
libhandler$ make VARIANT=release
libhandler$ make tests VARIANT=release
```

and copy the final library to the `out` directory:
```bash
libhandler$ cp out/gcc-amd64-linux-gnu/release/libhandler.a  out    # use platform specific path
```
(this is `out/gcc-amd64-apple-darwin19.6.0/release/libhandler.a` on macOS)

and move back up to the parent directory:
```bash
libhandler$ cd ..
```


## Libmprompt

Install [libmprompt] from the benchmark root directory (into the `libmprompt` sub-directory).
```
$ git clone https://github.com/koka-lang/libmprompt
$ cd libmprompt
```

Build the library:
```
libmprompt$ mkdir -p out/crelease            # must be `out/crelease`
libmprompt$ cd out/crelease
libmprompt/out/crelease$ cmake ../.. -DMP_USE_C=ON
libmprompt/out/crelease$ make
```

and move back up to the parent benchmark directory:
```
libmprompt/out/crelease$ cd ../../..
```


## Multi-core OCaml

Install Opam: <https://opam.ocaml.org/doc/Install.html>:
```
$ sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```

Install Multi-core OCaml with new parallel_minor_gc:
```
$ opam init    # if this is the first time running opam, and maybe you need to run: > eval $(opam env)
$ opam update  
$ opam switch create 4.10.0+multicore --packages=ocaml-variants.4.10.0+multicore --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default
$ eval $(opam env)  # ensure the right variant is used
```

Install Dune (and lwt (not sure if required for the benchmarks))
```
$ opam install dune lwt
```

Test installation:
```
$ ocamlopt --version
4.10.0+multicore
```

If it does not say `+multicore` you need to probably use:
```
$ eval $(opam env)
``` 
to switch to the right version. (Use `opam switch list` to see the
installed [variants](https://discuss.ocaml.org/t/how-does-one-switch-ocaml-version/4702))



# Building the Benchmarks

After installing the pre-requisites, the benchmarks should all build:
```
$ chmod +x ./build.sh
$ ./build.sh

...

--------------------------------------------------
run benchmarks as:

$ out/kk/bench

usage:
 out/kk/bench [options]

options:
 -t<test>  --test=test  comma separated list of tests
 -l<lang>  --lang=lang  comma separated list of languages
 -i<N>     --iter=N     use N (=1) iterations per test
 -c        --chart      generate latex chart
 -n        --norm       normalize results relative to Koka
 -h        --help       show this information

notes:
  tests    : counter, counter1, counter10, mstate, nqueens
  languages: kk, ml, mp, ev, hia, lh, lm, kknt, lmnt, base

all builds successful.
```


# Running the Benchmarks

Run all benchmarks by running the `out/kk/bench` program that was build
in the previous step:
```
$ ./out/kk/bench 
``` 

Median over 10 runs with LaTeX graph:
```
$ ./out/kk/bench -i10 -c
``` 

Run a particular benchmark:
```
$ ./out/kk/bench --test=counter,nqueens
``` 

Run for a particular language:
```
$ ./out/kk/bench --test=counter --lang=c,kk,ev
``` 

When generating an LaTeX graph (`-c`), you can copy-and-paste the 
LaTeX (in between the `~ begin snippet` blocks) 
into the `graph/bench.tex` file, and then use 
```
$ cd graph
graph$ pdflatex graph.tex
``` 
to generate a nice bar graph.


[koka]:       https://koka-lang.github.io
[libhandler]: https://github.com/koka-lang/libhandler#readme
[libmprompt]: https://github.com/koka-lang/libmprompt#readme
[Ev.Eff]:     https://github.com/xnning/eveff#readme
[Mp.Eff]:     https://github.com/xnning/mpeff#readme
[hia]:        https://github.com/slindley/effect-handlers
[mcocaml]:    https://github.com/ocaml-multicore/ocaml-multicore
