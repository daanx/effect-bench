#!/bin/bash
#---------------------------------------------------------------------------
# Copyright 2021 <anonymous>
#---------------------------------------------------------------------------

echo "--- Build Benchmarks ---"
echo ""
echo "Use '-h' or '--help' for help on configuration options."
echo ""

benchmarks="counter,counter1,counter10,mstate,nqueens,triple" 

dobuild="yes"
rebuild="no"
rebuildlmp="no"
verbose="no"
ccomp="$CC"
kokac="koka-v2.1.2"

#kokac="stack --work-dir=~/home/dev/koka exec koka -- "
#kokac="/mnt/c/Users/daan/dev/koka/.stack-work/install/x86_64-linux-tinfo6/431d4e55f4d2e20c6f6be7ead520a8d7bf88ba5fd63586d6e397109ab1b784f0/8.8.4/bin/koka"

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
makedir "out/kkins"   
makedir "out/kkscut"
makedir "out/kkbinl"
makedir "out/ml"
makedir "out/base"
makedir "out/lh"
makedir "out/lhnt"
makedir "out/lmp"
makedir "out/lmpnt"
makedir "out/lmpw"
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
makedir "out/lcap"
makedir "out/llcap"


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
function build_kk {  # source_noext variant ("","nt","ins")
  if [ "$2" != "" ]; then
    if [ "$2" == "nt" ]; then
      koka_comp="$kokac"  # use regular compiler, non-tail is done in source 
    else
      koka_comp="$kokac-$2"
    fi      
    koka_ext=".$2.kk"
  else
    koka_comp="$kokac"
    koka_ext=".kk"
  fi
  koka_source="$1$koka_ext"
  if [ -f $koka_source ]; then  # often we can reuse an existing kk source for compiler variants
    base=`basename $koka_source $koka_ext`    
  else
    if [ -f $1.kk ]; then
      koka_source="$1.kk"
      base=`basename $koka_source .kk`
    else 
      base=""
    fi
  fi
  echo 
  echo "-- build $koka_source ($2) ---------------------------------"
  if [ "$base" != "" ]; then
    if [ "$koka_source" -nt "out/kk$2/$base" ]; then      
      echo_cmd $1 "$koka_comp -c -O2 --builddir=out/kk$2 -isrc -o $base $koka_source"
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

function build_base_ml {  # bench subdir
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .$2.ml`
    if [ "$1" -nt "out/$2/$base" ]; then
      pushd out/$2
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
      flto="-flto"
      if [ "$base" = "mstate" ]; then  # with flto the tail-call is not optimized :-(
        flto=""
      fi
      echo_cmd $1 "$ccomp -O3 $flto -o out/lmp/$base -Ilibmprompt/include  $1 libmprompt/out/crelease/libmpeff.a -lpthread"
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
      flto="-flto"
      if [ "$base" = "mstate" ]; then # with flto the tail-call is not optimized :-(
        flto=""
      fi
      echo_cmd $1 "$ccomp -O3 $flto -o out/lmpnt/$base -Ilibmprompt/include  $1 libmprompt/out/crelease/libmpeff.a  -lpthread"
    else
      echo "  up to date; skip re-compilation"
    fi
  else
    echo "  not found; skipping."
  fi
}

function build_lmpw {
  echo 
  echo "-- build $1 ---------------------------------"
  if test -f $1; then
    base=`basename $1 .lmpw.c`
    if [ "$1" -nt "out/lmpw/$base" ]; then
      flto="-flto"
      if [ "$base" = "mstate" ]; then  # with flto the tail-call is not optimized :-(
        flto=""
      fi
      echo_cmd $1 "$ccomp -O3 $flto -o out/lmpw/$base -Ilibmprompt/include  $1 libmprompt/out/crelease/libmpwasm.a -lpthread"
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
    build_kk "$bench" ""
    build_kk "$bench" "nt"
    build_kk "$bench" "ins"
    build_kk "$bench" "scut"
    build_kk "$bench" "binl"

    build_ml     "$bench.ml"
    #build_base_ml "$bench.base.ml"  "base"
    #build_base_ml "$bench.lcap.ml"  "lcap"
    #build_base_ml "$bench.llcap.ml" "llcap"
    build_mp     "$bench.mp.hs"
    #build_mp_nt  "$bench.nt.mp.hs"
    build_ev     "$bench.ev.hs"
    #build_ev_nt  "$bench.nt.ev.hs"
    build_hia    "$bench.hia.hs"
    build_lh     "$bench.lh.c"
    #build_lh_nt  "$bench.nt.lh.c"
    #build_lmp    "$bench.lmp.c"
    #build_lmp_nt "$bench.nt.lmp.c"
    #build_lmpw   "$bench.lmpw.c"
  done
fi

# build benchmark script
build_kk "bench" ""

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

