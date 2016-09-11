#!/bin/zsh

echo "Cleaning stuff..."
rm files/*.out

echo "Compiling machine.hs..."
ghc --make src/nbody3d.hs

cd ./src/

for d in ../files/*.in
do
    echo "Analyzing file $d... writing output to $d.out"
    cat $d | ./nbody3d >> $d.out
done

cd ..
echo "All files analyzed"
rm src/nbody3d.hi src/nbody3d.o src/nbody3d

exit
