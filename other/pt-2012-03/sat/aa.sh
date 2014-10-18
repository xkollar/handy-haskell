#!/bin/bash

name=${name:-lev3}
satsolver=minisat2

[ -f $name.cnf ] && {
    echo "$name.cnf already generated"
} || {
    echo -n "Generating $name.cnf ... main ... "
    ghc --make -O3 -o main kwak.hs
    ./main -g $name.cnf
    echo -n '... wc ... '
    clausenum=$(( $( wc -l $name.cnf | cut -d ' ' -f '1' ) -1 ))
    echo -n '... sed ... '
    sed -i.orig "1{s/_/${clausenum}/}" $name.cnf
    echo 'Done.'
}
[ -f $name-res.txt ] && {
    echo reult already computed
} || {
    $satsolver $name.cnf $name-res.txt
}
if grep '^SAT' $name-res.txt; then
    runghc kwak.hs -d $name-res.txt
else
    echo 'Unsatisfiable :-('
fi

