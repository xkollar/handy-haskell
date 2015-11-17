#!/usr/bin/env bash
# Re-generate documentation for gh-pages.
# Should be run from directory above the one with
# handy-haskell.cabal file.

set -eu

tmp=$( mktemp -d )
echo "${tmp}"

cd handy

cabal sandbox init
cabal configure
cabal build
cabal haddock --hyperlink-source

mv dist/doc/html/handy "${tmp}"
find "${tmp}" -name '*.html' -print0 \
| xargs -0 -n1 sed -i 's/"file:\/\/\/[^"]*\/doc\/ghc\/html\/libraries\/\([^/]*\)\//"https:\/\/hackage.haskell.org\/package\/\1\/docs\//g'

cabal clean
cabal sandbox delete

cd ..

git checkout gh-pages || git checkout -b gh-pages origin/gh-pages
git pull --rebase


git ls-tree -r --name-only gh-pages -z \
| xargs -0 rm

cp -r "${tmp}"/handy .

read -p "Continue... 4"

git add -f handy
git commit -v --amend
git checkout master

rm -rf "${tmp}"
