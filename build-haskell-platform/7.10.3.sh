#!/usr/bin/env bash
# Checks for packages and builds binary haskell platfrom.

set -eu

if [ x"$( uname -i )" = x"x86_64" ]; then
    declare -r GHC_URL='https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-deb8-linux.tar.xz'
else
    declare -r GHC_URL='https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-i386-deb8-linux.tar.xz'
fi
declare -r GHC_ARCHIVE="$( basename "${GHC_URL}" )"
declare -r GHC_VERSION_STRING='The Glorious Glasgow Haskell Compilation System, version 7.10.3'

declare -r CABAL_URL='https://www.haskell.org/cabal/release/cabal-install-1.18.1.0/cabal-install-1.18.1.0.tar.gz'
declare -r CABAL_VERSION_STRING=$'cabal-install version 1.18.1.0\nusing version 1.18.1.5 of the Cabal library '

function fetch_url() {
    local -r url=${1}; shift
    local -r file=${1}; shift

    if test -f "${file}"; then
        return
    fi

    wget -O "${file}" "${url}"
}

function main() {
    fetch_url "${GHC_URL}" "${GHC_ARCHIVE}"

    ls
}

main "${@}"
