#!/bin/bash
#---------------------------------------------------------------------------
# Copyright 2021, Daan Leijen.
#---------------------------------------------------------------------------

echo "--- Build Benchmarks ---"
echo ""
echo "Use '-h' or '--help' for help on configuration options."
echo ""

benchmarks="counter,counter1,counter10,mstate,nqueens" # ,rcounter,rcounter1,rcounter10"

dobuild="yes"
rebuild="no"
rebuildlmp="no"
verbose="no"
ccomp="$CC"

if [ -z "$ccomp" ]; then
  ccomp="gcc -g"
fi

function makedir {
  if ! [ -d "$1" ]; then
    mkdir $1
  fi
}

curdir=`pwd`
makedir "out"
makedir "out/kk"
makedir "out/kknt"
makedir "out/ml"
makedir "out/base"
makedir "out/lh"
makedir "out/lhnt"
makedir "out/lmp"
makedir "out/lmpnt"
makedir "out/ev"
#cp -f mpeff/Control/Ev/*.hs out/ev
makedir "out/evnt"
#cp -f mpeff/Control/Ev/*.hs out/evnt
makedir "out/mp"
cp -f mpeff/src/Control/Mp/*.hs out/mp
makedir "out/mpnt"
cp -f mpeff/src/Control/Mp/*.hs out/mpnt
makedir "out/hia"
cp -f effect-handlers/*.hs out/hia

# Parse command-line arguments
while : ; do
  flag=$1
  case "$flag" in
  *=*)  flag_arg="${flag#*=}";;
  *)    flag_arg="yes" ;;
  esac
  echo "option: $flag, arg: $flag_arg"
  case "$flag" in
    "") break;;
    --build)
        dobuild="yes";;
    --rebuild)
        rebuild="yes";;
    --rebuildlmp)
        rebuildlmp="yes";;
    -b|--bench=*)
        benchmarks="$flag_arg";;
    -v|--verbose)
        verbose="yes";;
    -h|--help|-\?|help|\?)
        echo "./bench [options]"
        echo ""
        echo "  --verbose                    be verbose"
        echo "  --build                      build benchmarks"
        echo ""
        echo ""
        exit 0;;
    *) echo "warning: unknown option \"$1\"." 1>&2
  esac
  shift
done

export verbose

errors=""

# benchmark list to array
IFS=',' read -ra benches <<< "$benchmarks"    #comma seperated string to array `benches`

function echo_cmd {
  echo $2
  $2
  if [ $? -ne 0 ]; then
    errors="$errors; $1"
    echo "ERROR: $?"
  else 
    echo "ok."
  fi
}


if [ "$rebuildlmp" = "yes" ]; then 
  rm -rf out/lmp/* out/lmpnt/*
  pushd libmprompt/out/crelease
  make
  popd
fi

if [ "$rebuild" = "yes" ]; then
  rm -rf out/*  
fi

# -------------------------------------------------------------
# building
function build_kk {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .kk`
    if [ "$1" -nt "out/kk/$base" ]; then
      echo_cmd $1 "koka -c -O2 --builddir=out/kk -isrc -o $base $1"
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_kk_nt {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .nt.kk`
    if [ "$1" -nt "out/kknt/$base" ]; then
      echo_cmd $1 "koka -c -O2 --builddir=out/kknt -isrc -o $base $1"
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_ml {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .ml`
    if [ "$1" -nt "out/ml/$base" ]; then
      pushd out/ml
      cp -f ../../$1 $base.ml
      echo_cmd $1 "ocamlopt -O3 -o $base $base.ml"
      popd
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_base_ml {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .base.ml`
    if [ "$1" -nt "out/base/$base" ]; then
      pushd out/base
      cp -f ../../$1 $base.ml
      echo_cmd $1 "ocamlopt -O3 -o $base $base.ml"
      popd
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_mp {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .mp.hs`
    if [ "$1" -nt "out/mp/$base" ]; then
      pushd out/mp
      cp -f ../../$1 $base.hs
      echo_cmd $1 "ghc -O2 -fspec-constr-keen -package primitive -o $base Eff.hs Util.hs $base.hs"
      popd
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_mp_nt {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .mp.nt.hs`
    if [ "$1" -nt "out/mpnt/$base" ]; then
      pushd out/mpnt
      cp -f ../../$1 $base.hs
      echo_cmd $1 "ghc -O2 -fspec-constr-keen -package primitive -o $base Eff.hs Util.hs $base.hs"
      popd
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_ev {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .ev.hs`
    if [ "$1" -nt "out/ev/$base" ]; then
      pushd out/ev
      cp -f ../../$1 $base.hs
      # echo_cmd $1 "ghc -O2 -fspec-constr-keen -package primitive -package eveff -o $base $base.hs"
      echo_cmd $1 "ghc -O2 -fspec-constr-keen -package primitive -package eveff -o $base  $base.hs"
      popd
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_ev_nt {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .nt.ev.hs`
    if [ "$1" -nt "out/evnt/$base" ]; then
      pushd out/evnt
      cp -f ../../$1 $base.hs
      # echo_cmd $1 "ghc -O2 -fspec-constr-keen -package primitive -o $base $base.hs"
      echo_cmd $1 "ghc -O2 -fspec-constr-keen -package primitive -package eveff -o $base  $base.hs"
      popd
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_lh {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .lh.c`
    if [ "$1" -nt "out/lh/$base" ]; then
      echo_cmd $1 "$ccomp -O3 -o out/lh/$base -Ilibhandler/inc  $1 libhandler/out/libhandler.a"
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_lh_nt {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .nt.lh.c`
    if [ "$1" -nt "out/lhnt/$base" ]; then    
      pushd out/lhnt
      cp ../../$1 $base.c
      echo_cmd $1 "$ccomp -O3 -o $base -I../../libhandler/inc  $base.c ../../libhandler/out/libhandler.a"
      popd
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}


function build_lmp {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .lmp.c`
    if [ "$1" -nt "out/lmp/$base" ]; then
      echo_cmd $1 "$ccomp -O3 -o out/lmp/$base -Ilibmprompt/include  $1 libmprompt/out/crelease/libmphandler.a libmprompt/out/crelease/libmprompt.a -lpthread"
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_lmp_nt {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .nt.lmp.c`
    if [ "$1" -nt "out/lmpnt/$base" ]; then
      echo_cmd $1 "$ccomp -O3 -o out/lmpnt/$base -Ilibmprompt/include  $1 libmprompt/out/crelease/libmphandler.a libmprompt/out/crelease/libmprompt.a -lpthread"
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_hia {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .hia.hs`
    if [ "$1" -nt "out/hia/$base" ]; then
      pushd out/hia
      cp -f ../../$1 $base.hs      
      echo_cmd $1 "ghc -O2 -package haskell-src-exts -package parsec -o $base  $base.hs"
      popd
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

# build each benchmark
if test $dobuild = "yes"; then
  for benchname in "${benches[@]}"; do
    bench="src/$benchname"
    case "$bench" in
      *.nt.kk)    build_kk_nt $bench;;
      *.kk)       build_kk $bench;;
      *.base.ml)  build_base_ml $bench;;
      *.ml)       build_ml $bench;;
      *.nt.ev.hs) build_ev_nt $bench;;
      *.ev.hs)    build_ev $bench;;
      *.nt.mp.hs) build_mp_nt $bench;;
      *.mp.hs)    build_mp $bench;;      
      *.nt.lmp.c)  build_lmp_nt $bench;;
      *.lmp.c)     build_lmp $bench;;
      *.nt.lh.c)  build_lh_nt $bench;;
      *.lh.c)     build_lh $bench;;
      *.hia.hs)   build_hia $bench;;      
      *)    build_kk     "$bench.kk"
            build_kk_nt  "$bench.nt.kk"
            build_base_ml "$bench.base.ml"
            build_ml     "$bench.ml"
            build_mp     "$bench.mp.hs"
            build_mp_nt  "$bench.nt.mp.hs"
            build_ev     "$bench.ev.hs"
            build_ev_nt  "$bench.nt.ev.hs"
            build_hia    "$bench.hia.hs"
            build_lh     "$bench.lh.c"
            build_lh_nt  "$bench.nt.lh.c"
            build_lmp     "$bench.lmp.c"
            build_lmp_nt  "$bench.nt.lmp.c";;
    esac
  done
fi

# build benchmark script
build_kk "bench.kk"

echo ""
echo "--------------------------------------------------"
echo "run benchmarks as:"
echo ""
echo "> out/kk/bench"
echo ""
out/kk/bench -h

echo ""
if [ "$errors" = "" ]; then
  echo "all builds successful."
else  
  echo "errors encountered: $errors"
fi

echo ""

