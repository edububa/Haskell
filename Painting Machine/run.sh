#!/bin/zsh

echo "Cleaning stuff..."
rm files/*.out

echo "Compiling machine.hs..."
ghc --make src/machine.hs

cd ./src/



for d in ../files/*.in
do
    echo "Analyzing file $d... writing output to $d.out"
    cat $d | ./machine >> $d.out
done

cd ..
echo "All files analyzed"
rm src/machine.hi src/machine.o src/machine

exit
