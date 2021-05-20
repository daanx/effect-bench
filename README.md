pre {
  font-size: small;
}

# Introduction

Section 5, Figure 6 of the paper contains the benchmarks.
First pull and run the provided docker image:
```
$ docker pull daanx/icfp21-multip-artifact:1.1
$ docker run -it -w/root/effect-bench-artifact daanx/icfp21-multip-artifact:1.1
```
We now see the docker prompt, and the benchmarks can now be run as:
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

The tested systems in the paper are:

- `kk` (extension `.kk`), [Koka] 2.1.2.
- `ml` (extension `.ml`), [multi-core OCaml][mcocaml] 4.10.1.
- `mp` (extension `.mp.hs`), [Mp.Eff] Haskell library.
- `ev` (extension `.ev.hs`), [Ev.Eff] Haskell library.
- `lh` (extension `.lh.c`), [libhandler] C library (v0.5).
- `kkins` (extension `.ins.kk`), Koka with insertion ordered evidence.
- `kkscut` (extension `.scut.kk`), Koka without short-cut resumptions.
- `kkbinl` (extension `.binl.kk`), Koka without bind-inlining.
- `kknt` (extension `.nt.kk`) Koka without tail-resumptive optimization.

You can see sample output in the next Section ([#sec-sample]).

To make the benchmarking between systems as fair as possible we adjust each 
benchmark to fit the target system. In particular:

- We use native ints when possible (`Int` in Haskell, `int` in ML and C, and
  `int32` in Koka)
- Koka uses sometimes `rcontrol` (raw control) to avoid running finally
  clauses automatically (in `mstate`)

The benchmarks folder consists of:

* `/src`: the sources for all the benchmarks. Each benchmark
  has an extension for each variant (for example `counter.ml`, or `counter1.ev.hs`).
  For the internal Koka variants most of the time the `.kk` source is used
  unless a specific version exists (like `counter.nt.kk` for non tail-resumptive optimization).
  
  Each benchmark is designed to test effect handling aspects specifically and
  minimize any other computation. These consist of:

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
  - `triple` uses multi-shot resumptions to search for Pythagorean triples. As
    such, it behaves similar to `nqueens`.

* `/out`: the output folder for all benchmarks. The benchmarks can be
  re-build by invoking `./build.sh` (see below for instructions if needed)

  - `/out/kk/bench`: the benchmark program compiled from `bench.kk`.
    This runs all benchmarks and calculates the median of the timings.
  - `/out/<variant>`: the generated benchmark programs for a particular
    variant. For the Koka compiler variants, one can inspect the 
    generated C code here, and for example see the bind-inlining at
    work by comparing `/out/kk/counter.c` with `/out/kkbinl/counter.c`.

* `/graph`: helper folder to generate a bar graph from test results.

* `/libhandler`: sources for the [libhandler] C library (v0.5).

* `/mpeff`: sources for the [Mp.Eff] library.

* `/koka`: sources for the [Koka] compiler. Checked out from tag `v2.1.2`.
  From this the standard Koka is build (`koka-v2.1.2`), as well
  as the variants `koka-v2.1.2-scut`, `koka-v2.1.2-ins`, and `koka-v2.1.2-binl`.
  (we do not need a `kknt` variant as that is done in the benchmark sources
  and needs to changes to the compiler internally).
  The variants were build by checking out the branches `artifact/scut`,
  `artifact/evins`, and `artifact/binl` respectively (see below for build 
  instructions if needed).


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


# Installing From Scratch

We highly recommend the use of the Docker image as an install from scratch is a
lot work due to all the different systems required. 

The following commands were used to prepare the Docker image, this is tested on
Ubuntu 20.04 and macOS Catalina. Follow the installation instructions carefully.

First checkout the effect benchmarks suite:
```
$ git clone https://github.com/daanx/effect-bench -b artifact
```
(or checkout commit `94f095a` instead of the `artifact` branch)
The rest of the installation should be run inside the `effect-bench` directory.
```
$ cd effect-bench
```

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
$ sudo apt-get install ghc cabal-install      
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.6.5 
```
(on macOS, use brew, like `$ brew install ghc cabal-install`)

Install the `primitive` and `eveff` package:
```
$ cabal update
$ cabal install primitive eveff    
$ cabal info primitive eveff | grep installed:
Versions installed: 0.7.1.0  
Versions installed: 0.1.0.0  
```

## Ev.Eff

Already installed as the package `eveff`


## Mp.Eff

Clone from the benchmark root directory (into the `mpeff` sub-directory).
```
$ git clone https://github.com/xnning/mpeff
```

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
libhandler$ cp out/gcc-amd64-linux-gnu/release/libhandler.a out
```
(use a platform specific path,
 this is `out/gcc-amd64-apple-darwin19.6.0/release/libhandler.a` on macOS)

and move back up to the parent directory:
```bash
libhandler$ cd ..
```


## Multi-core OCaml

Install Opam: <https://opam.ocaml.org/doc/Install.html>:
```
$ sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```
You may need to run:
```
$ eval $(opam env)
```
to initialize the Opam environment. Install Multi-core OCaml with new parallel_minor_gc:
```
$ opam init   
$ opam update  
$ opam switch create 4.10.0+multicore 
    --packages=ocaml-variants.4.10.0+multicore 
    --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default
$ eval $(opam env)
```

(note: If installing in a docker container (or WSL) you may need to disable sandboxing,
see <https://github.com/ocaml/opam/issues/3634>)

Install Dune 
```
$ opam install dune 
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
koka$ stack exec koka -- util/bundle      
koka$ util/install.sh -b dist/koka-v2.1.2-<variant>-linux-amd64.tar.gz    
koka$ git reset --hard HEAD               
```
Notes:

- Answer `No` if asked to uninstall a previous version!
- The first `util/bundle` build may fail in the `scut` variant, run it again.
- On macOSX, the bundle name ends with `-osx-amd64.tar.gz`
- The git `reset` is needed in case the cabal file changes.

And go back up again:
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

