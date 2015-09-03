#!/usr/bin/env bash
# Checks for packages and builds binary haskell platfrom.

set -eu

function check_package() {
    rpm -q "$1" || exit 1
}

GHC_URL="https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-i386-unknown-linux-deb7.tar.xz"
GHC_ARCHIVE="$( basename "${GHC_URL}" )"
GHC_BUILD_DIR=$( sed 's/\(ghc-[0-9.]*\)-.*/\1/' <<< "$GHC_ARCHIVE" )

# TMP_PREFIX=$( mktemp -d )
TMP_PREFIX=/tmp/ghc-kwak/
mkdir -p "${TMP_PREFIX}"
PATH="${TMP_PREFIX}/bin:${PATH}"

check_package gmp-devel
check_package gcc
check_package zlib-devel
check_package mesa-libGL-devel
check_package mesa-libGLU-devel
check_package freeglut-devel

export HOME="${TMP_PREFIX}/home1"

if ! test -f "${GHC_ARCHIVE}"; then
    wget "${GHC_URL}"
fi

if ! test -d "${GHC_BUILD_DIR}"; then
    tar xvJf "${GHC_ARCHIVE}"
fi

if ! [ x"$( ghc --version )" = x'The Glorious Glasgow Haskell Compilation System, version 7.8.4' ]; then
    cd "${GHC_BUILD_DIR}"
    ./configure --prefix="$TMP_PREFIX"
    make install
    cd -
fi

if ! test -f cabal-install-1.18.1.0.tar.gz; then
    wget "https://www.haskell.org/cabal/release/cabal-install-1.18.1.0/cabal-install-1.18.1.0.tar.gz"
fi

if ! test -d cabal-install-1.18.1.0; then
    tar xvzf "cabal-install-1.18.1.0.tar.gz"
fi

(
    cd cabal-install-1.18.1.0
    PREFIX=$TMP_PREFIX ./bootstrap.sh
    cabal update
    cabal install --prefix="${TMP_PREFIX}" hscolour
)

export HOME="${TMP_PREFIX}/home2"
mkdir -p "${HOME}"
cabal update

if ! test -d haskell-platform; then
    git clone https://github.com/trskop/haskell-platform.git
    (
        cd haskell-platform
        git reset --hard remotes/origin/ghc-7.8.4
    )
fi

# echo 'Fix haskell-platform/hptool/hptool.cabal to contain shake < 0.15'
# bash

(
    cd haskell-platform
    ./platform.sh --prefix="/opt/" "../${GHC_ARCHIVE}"
    # ./platform.sh "../${GHC_ARCHIVE}"
)

# rm -rf "${TMP_PREFIX}"
