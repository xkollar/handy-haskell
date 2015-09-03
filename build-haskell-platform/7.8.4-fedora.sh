#!/usr/bin/env bash
# Checks for packages and builds binary haskell platfrom.

set -eu

declare -r GHC_URL='https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-i386-unknown-linux-deb7.tar.xz'
declare -r GHC_ARCHIVE="$( basename "${GHC_URL}" )"
declare -r GHC_BUILD_DIR=$( sed 's/\(ghc-[0-9.]*\)-.*/\1/' <<< "${GHC_ARCHIVE}" )
declare -r GHC_VERSION_STRING='The Glorious Glasgow Haskell Compilation System, version 7.8.4'

declare -r CABAL_URL='https://www.haskell.org/cabal/release/cabal-install-1.18.1.0/cabal-install-1.18.1.0.tar.gz'

declare -r -a REQUIRED_PACKAGES=(
    gmp-devel
    gcc
    zlib-devel
    mesa-libGL-devel
    mesa-libGLU-devel
    freeglut-devel
    )

function check_package() {
    return
    rpm -q "${1}" || exit 1
}

CACHE_DIR=_cache

function cached_fetch() {
    url="${1}"
    dst="${2}"
    cache_path="${CACHE_DIR}/$( sha1sum <<< ${url} | sed -n 's/\s.*//p;q' )"

    if ! [ -e "${dst}" ]; then
        if ! [ -e "${cache_path}" ]; then
            mkdir -p "${CACHE_DIR}"
            wget "${url}" -O "${cache_path}"
            echo "${url}" > "${cache_path}.url"
        else
            echo "Using cache for ${url}"
        fi
        ln "${cache_path}" "${dst}"
    else
        echo "Not overriding ${dst}"
    fi
}

function main() {
    for pkg in "${REQUIRED_PACKAGES[@]}"; do
        check_package "${pkg}"
    done

    cached_fetch "${GHC_URL}" "${GHC_ARCHIVE}"
    if ! test -d "${GHC_BUILD_DIR}"; then
        mkdir -p "${GHC_BUILD_DIR}"
        tar -xvJ --strip-components=1 -C "${GHC_BUILD_DIR}" -f "${GHC_ARCHIVE}"
    fi

    # local -r TMP_PREFIX=$( mktemp -d )
    local -r TMP_PREFIX="${PWD}/_tmp_prefix"
    PATH="${TMP_PREFIX}/bin:${PATH}"

    export HOME="${TMP_PREFIX}/home1"
    mkdir -p "${TMP_PREFIX}"

    if ! [ x"$( ghc --version )" = x"${GHC_VERSION_STRING}" ]; then
        cd "${GHC_BUILD_DIR}"
        ./configure --prefix="${TMP_PREFIX}"
        make install
        cd -
    fi

    cached_fetch "${CABAL_URL}" 'cabal-install.tar.gz'
    if ! test -d 'cabal-install'; then
        mkdir -p "cabal-install"
        tar -xvz --strip-components=1 -C 'cabal-install' -f 'cabal-install.tar.gz'
    fi

    (
        cd 'cabal-install'
        PREFIX="${TMP_PREFIX}" ./bootstrap.sh
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

    (
        cd haskell-platform
        ./platform.sh --prefix="/opt/" "../${GHC_ARCHIVE}"
        # ./platform.sh "../${GHC_ARCHIVE}"
    )
    # rm -rf "${TMP_PREFIX}"
}

main "${@}"
