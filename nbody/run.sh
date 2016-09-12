#!/bin/zsh

echo "Cleaning stuff..."
rm files/*.out

cd ./src/

echo "Compiling machine.hs..."
ghc nbody.hs

if [ -f ./Forces/TwoD.o ]
then
    echo "Analyzing 2d inputs:"
    for d in ../files/*2d*.in
    do
        echo "Analyzing file $d... writing output to $d.out"
        cat $d | ./nbody >> $d.out
    done
    rm ./Forces/TwoD.o ./Forces/TwoD.hi
fi

if [ -f ./Forces/ThreeD.o ]
then
    echo "Analyzing 3d inputs:"
    for d in ../files/*3d*.in
    do
        echo "Analyzing file $d... writing output to $d.out"
        cat $d | ./nbody >> $d.out
    done
    rm ./Forces/ThreeD.o ./Forces/ThreeD.hi
fi

cd ..
echo "All files analyzed"
rm src/nbody.hi src/nbody.o src/nbody

exit
