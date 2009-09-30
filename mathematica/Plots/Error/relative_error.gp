#!/usr/bin/gnuplot -persist
#test.dat = (t,log(df)/log(10))

set title "Relative error for numerical folding (from linear estimate)"

set xlabel "Iteration number"
set ylabel "Log_{10}"

fit a*x+b "test.dat" via a,b

plot [0:400] "f_x_T2._.dat" u 1:(log(abs(10**(a*$1+b)/$2))/log(10)) t "Relative folding error\n(log(df/f))"
