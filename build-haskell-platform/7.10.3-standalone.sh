#!/usr/bin/env bash

set -eu

wget http://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-i386-deb8-linux.tar.xz
tar xvJf ghc-7.10.3-i386-deb8-linux.tar.xz
(
    cd ghc-7.10.3
    ./configure --prefix=/opt/ghc-7.10.3-i386
    sudo make install
    cd ..
)
rm -rf ghc-7.10.3

wget https://www.haskell.org/cabal/release/cabal-install-1.22.7.0/cabal-install-1.22.7.0.tar.gz
tar xvzf cabal-install-1.22.7.0.tar.gz
(
    cd cabal-install-1.22.7.0
    sudo env PREFIX="/opt/ghc-7.10.3-i386" PATH="/opt/ghc-7.10.3-i386/bin:${PATH}" ./bootstrap.sh --global
    cd ..
)
sudo rm -rf cabal-install-1.22.7.0
