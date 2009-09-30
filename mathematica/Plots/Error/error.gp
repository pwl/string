#!/usr/bin/gnuplot -persist
#test.dat = (t,log(df)/log(10))

set title "Numerical folding vs its error"

set xlabel "Iteration number"
set ylabel "Log_{10}"

fit a*x+b "test.dat" via a,b

plot [0:400] "test.dat" t "Folding error", a*x+b t "Error estimate via linear fit"
replot "f_x_T2._.dat" u 1:(log(abs($2))/log(10)) t "Numerical folding"
