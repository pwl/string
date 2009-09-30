#!/bin/bash

datafiles=$(find Results/Data/ -name "*.dat")

echo -ne "#!/usr/bin/gnuplot -persist\n\nplot " > plotter.gp
for f in $(find Results/Data/ -name "*.dat"|tail -n +2| head -n -1); do
    echo -ne " \"$f\" u 1:2 w l," >> plotter.gp
done

echo -ne " \"$(find Results/Data/ -name "*.dat"|tail -n -1)\" u 1:2 w l\n" >> plotter.gp

echo

chmod a+x plotter.gp
./plotter.gp

