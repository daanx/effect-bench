# Introduction

Section 5, Figure 6 of the paper contains the benchmarks.
First start the provided docker image:
```
$ docker run -it -w/root/effect-bench-artifact test-multip-icfp21-artifact
```

The benchmarks can now be run as:
```
root:~/effect-bench-artifact$ out/kk/bench
```

## Benchmarking

For the paper we ran each benchmark for 10 iterations but this will take some time
(on an AMD5950x most benchmarks finish under 10s, but `counter10` can take up
to 1 minute without tail-resumptive optimization (`kknt`)):
```
root:~/effect-bench-artifact$ out/kk/bench -i10
```

Once the tests have finished, the  median absolute times are displayed as well
as the error margin, and normalized results (Only the absolute times and error
margins are used in the paper). See the next section ([#sec-sample]) for sample output
on an AMD5950x -- results will depend on the system used but the relative
performance should be similar.

If needed, you can run a particular benchmark as:
```
root:~/effect-bench-artifact$ ./out/kk/bench --test=counter,nqueens
``` 

Or run for a particular language/system as:
```
root:~/effect-bench-artifact$ ./out/kk/bench --test=counter --lang=c,kk,ev
``` 

## Benchmark Overview

* The tested systems in the paper are:

  - `kk` (extension `.kk`), [Koka] 2.1.2 (the paper still has 2.0.16 but perf stayed the same).
  - `ml` (extension `.ml`), [multi-core OCaml][mcocaml] 4.10.1.
  - `mp` (extension `.mp.hs`), [Mp.Eff] Haskell library.
  - `ev` (extension `.ev.hs`), [Ev.Eff] Haskell library.
  - `lh` (extension `.lh.c`), [libhandler] C library (v0.5).
  - `kknt` (extension `.nt.kk`), Koka without tail-resumptive optimization.

* We were asked to also evaluate the impact of each optimization
  individually _within_ Koka. We performed these experiments where we
  evaluated the individual impact of each optimization, and we added:

  - `kkins` (extension `.ins.kk`), Koka with insertion ordered evidence 
  - `kkscut` (extension `.scut.kk`), Koka without short-cut resumptions 
  - `kkbinl` (extension `.binl.kk`), Koka without bind-inlining
  - (already present: `kknt` (extension `.nt.kk`) Koka without tail-resumptive
    optimization)

  As we can see at the sample output in the next Section ([#sec-sample])
  insertion-order shows the high linear search overhead in
  `count1`/`count10`; short-cut resumption offer a modest 10% improvement in
  `mstate`/`nqueens`, bind-inlining speeds up the `count` benchmarks by 25% and
  tail-resumptive optimization speeds up the `count` benchmarks by 10x.

* The benchmark sources are in the `src` directory and are all made to test
  effect handling aspects specifically and minimize any other computation. These consist of:

  - `counter`: tight loop with state effect handler doing a `get` and `put`
    (tail-resumptive) operation per iteration (100M). This tests the impact of
    tail-resumptive optimization.
  - `counter1`: as `counter` but with an unused reader handler in between. 
  - `counter10`: as `counter` but with 10 unused reader handlers in between.
    Together with `counter1` this measures the impact of nested effect handlers.
  - `mstate`: as `counter`, but using a monadic non-tail-resumptive state
    handler (and only 10M iterations). This demonstrates the use of full
    first-class resumptions captured under a lambda.
  - `nqueens`: the nqueens 12 problem using a multiple resumes to implement
    backtracking. This measures the impact of multi-shot resumptions.

* We were also asked by the reviewers to add the `triple` benchmark which uses multi-shot
  resumptions to search for pythogorean triples. As such, it behaves similar to `nqueens`.

* To make the benchmarking between systems as fair as possible we adjust each 
  benchmark to fit the target system. In particular:

  - We use native ints when possible (`Int` in Haskell, `int` in ML and C, and
    `int32` in Koka)
  - Koka uses sometimes `rcontrol` (raw control) to avoid running finally
    clauses automatically (in `mstate`)



# Sample output on an AMD5950x, Ubuntu 20.04  {#sec-sample;}

```
$ out/kk/bench
...

--- counter ----------------
counter,    kk,  1.14s ~0.000, 2600kb
counter,    ml,  1.05s ~0.000, 5140kb
counter,    mp,  4.74s ~0.000, 4456kb
counter,    ev,  0.26s ~0.000, 4364kb
counter,    lh,  0.72s ~0.000, 1540kb
counter, kkins,  1.21s ~0.000, 2580kb
counter, kkscut,  1.14s ~0.000, 2556kb
counter, kkbinl,  1.44s ~0.000, 2604kb
counter,  kknt, 11.99s ~0.000, 2908kb

--- counter1 ----------------
counter1,    kk,  1.16s ~0.000, 2628kb
counter1,    ml,  1.79s ~0.000, 5404kb
counter1,    mp,  4.61s ~0.000, 4424kb
counter1,    ev,  2.46s ~0.000, 4576kb
counter1,    lh,  0.81s ~0.000, 1528kb
counter1, kkins,  2.71s ~0.000, 2560kb
counter1, kkscut,  1.16s ~0.000, 2544kb
counter1, kkbinl,  1.44s ~0.000, 2612kb
counter1,  kknt, 18.36s ~0.000, 2912kb

--- counter10 ----------------
counter10,    kk,  1.13s ~0.000, 2600kb
counter10,    ml,  8.60s ~0.000, 5292kb
counter10,    mp,  5.58s ~0.000, 4424kb
counter10,    ev,  2.97s ~0.000, 4440kb
counter10,    lh,  1.93s ~0.000, 1532kb
counter10, kkins, 11.55s ~0.000, 2592kb
counter10, kkscut,  1.16s ~0.000, 2636kb
counter10, kkbinl,  1.43s ~0.000, 2708kb
counter10,  kknt, 54.72s ~0.000, 2924kb

--- mstate ----------------
mstate,    kk,  1.87s ~0.000, 2628kb
mstate,    ml,  0.39s ~0.000, 5104kb
mstate,    mp,  1.46s ~0.000, 4452kb
mstate,    ev,  0.58s ~0.000, 4372kb
mstate,    lh,  4.49s ~0.000, 1540kb
mstate, kkins,  1.81s ~0.000, 2616kb
mstate, kkscut,  2.03s ~0.000, 2528kb
mstate, kkbinl,  1.81s ~0.000, 2592kb
mstate,  kknt,  1.76s ~0.000, 2548kb

--- nqueens ----------------
nqueens,    kk,  0.77s ~0.000, 6084kb
nqueens,    ml,  2.55s ~0.000, 10556468kb
nqueens,    mp,  1.38s ~0.000, 14640kb
nqueens,    ev,  0.65s ~0.000, 12448kb
nqueens,    lh,  1.48s ~0.000, 28892kb
nqueens, kkins,  0.79s ~0.000, 5428kb
nqueens, kkscut,  0.87s ~0.000, 5352kb
nqueens, kkbinl,  0.80s ~0.000, 5420kb
nqueens,  kknt,  0.78s ~0.000, 5352kb

--- triple ----------------
triple,    kk,  1.08s ~0.000, 2584kb
triple,    ml,  2.54s ~0.000, 7123764kb
triple,    mp,  1.65s ~0.000, 4680kb
triple,    ev,  0.72s ~0.000, 4580kb
triple,    lh,  2.49s ~0.000, 1544kb
triple, kkins,  1.10s ~0.000, 2584kb
triple, kkscut,  1.05s ~0.000, 2684kb
triple, kkbinl,  1.14s ~0.000, 2580kb
triple,  kknt,  1.07s ~0.000, 2628kb
```


# Installing 

We highly recommend the use of the Docker image as an install from scratch is a
lot work due to all the different systems required. 

The following commands were used to prepare the Docker image, this is tested on
Ubuntu 20.04 and macOS Catalina. Follow the installation instructions carefully.


## Koka

Install [koka] v2.1.2:
```
$ curl -sSL https://github.com/koka-lang/koka/releases/download/v2.1.2/install.sh | sh
$ koka --version
Koka 2.1.2, 17:46:03 Mar  8 2021 (ghc release version)
```


## Haskell

Install Ghc 8.6.5 and Cabal:
```
$ sudo apt-get install ghc cabal-install      # on macOS: > brew install ghc cabal-install
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

(note: If installing in a docker container (or WSL) you may need to disable sandboxing,
see <https://github.com/ocaml/opam/issues/3634>)

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


## Koka Variants

For all the Koka compiler variants we need to build from special branches.
Get the Koka sources:
```
$ git clone --recursive https://github.com/koka-lang/koka
$ cd koka
```

Possibly install `stack`:
```
koka$ curl -sSL https://get.haskellstack.org/ | sh
```

Now build and install with each of the following branches: `evins`, `scut`, and `binl`, 
which create the variants `ins`, `scut`, and `binl` respectively.
```
koka$ git checkout artifact/<branch>
koka$ stack build
koka$ stack exec koka -- util/bundle      # may need to try twice for the scut variant
koka$ util/install.sh -b dist/koka-v2.1.2-<variant>-linux-amd64.tar.gz    # or -osx-amd64.tar.gz on macOS
koka$ git reset --hard HEAD               # reset in case the cabal file was modified
```
(and answer `No` if asked to uninstall a previous version!)

```
koka$ cd ..
```

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

