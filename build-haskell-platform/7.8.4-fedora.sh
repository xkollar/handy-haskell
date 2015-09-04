#!/usr/bin/env bash
# Checks for packages and builds binary haskell platfrom.

set -eu

if [ x"$( uname -i )" = x"x86_64" ]; then
    declare -r GHC_URL='https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz'
else
    declare -r GHC_URL='https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-i386-unknown-linux-deb7.tar.xz'
fi
declare -r GHC_ARCHIVE="$( basename "${GHC_URL}" )"
declare -r GHC_VERSION_STRING='The Glorious Glasgow Haskell Compilation System, version 7.8.4'

declare -r CABAL_URL='https://www.haskell.org/cabal/release/cabal-install-1.18.1.0/cabal-install-1.18.1.0.tar.gz'
declare -r CABAL_VERSION_STRING=$'cabal-install version 1.18.1.0\nusing version 1.18.1.5 of the Cabal library '

declare -r -a REQUIRED_PACKAGES=(
    gmp-devel
    gcc
    zlib-devel
    mesa-libGL-devel
    mesa-libGLU-devel
    freeglut-devel
    )

function check_package() {
    rpm -q "${1}" || exit 1
}

CACHE_DIR="${PWD}/_cache"

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

    local -r TMP_PREFIX="${PWD}/_tmp_prefix"
    PATH="${TMP_PREFIX}/bin:${PATH}"

    function prepare_ghc() {
        if type ghc && [ x"$( ghc --version )" = x"${GHC_VERSION_STRING}" ]; then
            return
        fi

        cached_fetch "${GHC_URL}" "${GHC_ARCHIVE}"

        local -r GHC_BUILD_DIR="${TMP_PREFIX}/$( sed 's/\(ghc-[0-9.]*\)-.*/\1/' <<< "${GHC_ARCHIVE}" )"

        if ! test -d "${GHC_BUILD_DIR}"; then
            mkdir -p "${GHC_BUILD_DIR}"
            tar -xvJ --strip-components=1 -C "${GHC_BUILD_DIR}" -f "${GHC_ARCHIVE}"
        fi

        (
            cd "${GHC_BUILD_DIR}"
            ./configure --prefix="${TMP_PREFIX}"
            make install
        )

        rm -rf "${GHC_BUILD_DIR}"
    }

    function prepare_cabal() {
        if type cabal && [ x"$( cabal --version )" = x"${CABAL_VERSION_STRING}" ]; then
            return
        fi

        cached_fetch "${CABAL_URL}" 'cabal-install.tar.gz'

        local -r CABAL_BUILD_DIR="${TMP_PREFIX}/cabal-install"

        if ! test -d "${CABAL_BUILD_DIR}"; then
            mkdir -p "${CABAL_BUILD_DIR}"
            tar -xvz --strip-components=1 -C "${CABAL_BUILD_DIR}" -f 'cabal-install.tar.gz'
        fi

        (
            cd "${CABAL_BUILD_DIR}"
            PREFIX="${TMP_PREFIX}" ./bootstrap.sh
        )

        rm -rf "${CABAL_BUILD_DIR}"

    }

    export HOME="${TMP_PREFIX}/home1"
    mkdir -p "${TMP_PREFIX}"

    prepare_ghc
    prepare_cabal

    cabal update
    cabal install --prefix="${TMP_PREFIX}" hscolour

    cached_fetch "${GHC_URL}" "${GHC_ARCHIVE}"

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
        ln build/product/haskell-platform-*.tar.gz ..
    )
    # rm -rf "${TMP_PREFIX}"
}

main "${@}"
